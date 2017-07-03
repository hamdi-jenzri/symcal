package com.github.symcal.javacompiler

import java.io.{File, IOException, _}
import java.net.{JarURLConnection, URI, URL}
import java.util.jar.JarEntry
import javax.lang.model.element.{Modifier, NestingKind}
import javax.tools.JavaFileObject.Kind
import javax.tools.{FileObject, JavaFileManager, JavaFileObject, StandardJavaFileManager, StandardLocation}

/**
  * @author atamur
  *         see http://atamur.blogspot.com/2009/10/using-built-in-javacompiler-with-custom.html
  * @since 15-Oct-2009
  */
class CustomClassloaderJavaFileManager(val classLoader: ClassLoader, val standardFileManager: StandardJavaFileManager) extends JavaFileManager {
  final private val finder: PackageInternalsFinder = new PackageInternalsFinder(classLoader)

  def getClassLoader(location: JavaFileManager.Location): ClassLoader = classLoader

  def inferBinaryName(location: JavaFileManager.Location, file: JavaFileObject): String = file match {
    case customFile: CustomJavaFileObject ⇒ customFile.binaryName
    case _ ⇒
      // if it's not CustomJavaFileObject, then it's coming from standard file manager - let it handle the file
      standardFileManager.inferBinaryName(location, file)
  }

  def isSameFile(a: FileObject, b: FileObject): Boolean = standardFileManager.isSameFile(a, b)

  def handleOption(current: String, remaining: java.util.Iterator[String]): Boolean = standardFileManager.handleOption(current, remaining)

  def hasLocation(location: JavaFileManager.Location): Boolean = {
    (location eq StandardLocation.CLASS_PATH) || (location eq StandardLocation.PLATFORM_CLASS_PATH) // we don't care about source and other location types - not needed for compilation
  }

  @throws[IOException]
  def getJavaFileForInput(location: JavaFileManager.Location, className: String, kind: JavaFileObject.Kind): JavaFileObject =
    standardFileManager.getJavaFileForInput(location, className, kind)

  @throws[IOException]
  def getJavaFileForOutput(location: JavaFileManager.Location, className: String, kind: JavaFileObject.Kind, sibling: FileObject): JavaFileObject =
    standardFileManager.getJavaFileForOutput(location, className, kind, sibling)

  @throws[IOException]
  def getFileForInput(location: JavaFileManager.Location, packageName: String, relativeName: String): FileObject =
    standardFileManager.getFileForInput(location, packageName, relativeName)

  @throws[IOException]
  def getFileForOutput(location: JavaFileManager.Location, packageName: String, relativeName: String, sibling: FileObject): FileObject =
    standardFileManager.getFileForOutput(location, packageName, relativeName, sibling)

  @throws[IOException]
  def flush() {
    // do nothing
  }

  @throws[IOException]
  def close() {
    // do nothing
  }

  @throws[IOException]
  def list(location: JavaFileManager.Location, packageName: String, kinds: java.util.Set[JavaFileObject.Kind], recurse: Boolean): java.lang.Iterable[JavaFileObject] = {
    if (location eq StandardLocation.PLATFORM_CLASS_PATH) {
      // let standard manager hanfle
      standardFileManager.list(location, packageName, kinds, recurse)
    } else if ((location eq StandardLocation.CLASS_PATH) && kinds.contains(JavaFileObject.Kind.CLASS)) {
      if (packageName.startsWith("java.") || packageName.startsWith("javax.")) {
        // a hack to let standard manager handle locations like "java.lang" or "java.util". Prob would make sense to join results of standard manager with those of my finder here
        standardFileManager.list(location, packageName, kinds, recurse)
      } else {
        // app specific classes are here
        finder.find(packageName)
      }
    } else
      java.util.Collections.emptyList()
  }

  def isSupportedOption(option: String): Int = -1
}

class CustomJavaFileObject(val binaryName: String, val uri: URI) extends JavaFileObject {

  // for FS based URI the path is not null, for JAR URI the scheme specific part is not null
  final val name: String = Option(uri.getPath).getOrElse(uri.getSchemeSpecificPart)

  def toUri: URI = uri

  override def getName: String = name

  @throws[IOException]
  def openInputStream: InputStream = uri.toURL.openStream() // easy way to handle any URI!

  @throws[IOException]
  def openOutputStream: OutputStream = throw new UnsupportedOperationException

  @throws[IOException]
  def openReader(ignoreEncodingErrors: Boolean): Reader = throw new UnsupportedOperationException

  @throws[IOException]
  def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = throw new UnsupportedOperationException

  @throws[IOException]
  def openWriter: Writer = throw new UnsupportedOperationException

  def getLastModified: Long = 0

  def delete: Boolean = throw new UnsupportedOperationException

  def getKind: JavaFileObject.Kind = Kind.CLASS

  // copied from SimpleJavaFileManager
  def isNameCompatible(simpleName: String, kind: JavaFileObject.Kind): Boolean = {
    val baseName: String = simpleName + kind.extension
    kind == getKind && (baseName == getName || getName.endsWith("/" + baseName))
  }

  def getNestingKind: NestingKind = throw new UnsupportedOperationException

  def getAccessLevel: Modifier = throw new UnsupportedOperationException

  override val toString: String = "CustomJavaFileObject{" + "uri=" + uri + '}'
}

object PackageInternalsFinder {
  private val CLASS_FILE_EXTENSION: String = ".class"
}

class PackageInternalsFinder(var classLoader: ClassLoader) {
  @throws[IOException]
  def find(packageName: String): java.util.List[JavaFileObject] = {
    val javaPackageName: String = packageName.replaceAll("\\.", "/")
    val result: java.util.List[JavaFileObject] = new java.util.ArrayList[JavaFileObject]
    val urlEnumeration: java.util.Enumeration[URL] = classLoader.getResources(javaPackageName)
    while (urlEnumeration.hasMoreElements) {
      // one URL for each jar on the classpath that has the given package
      val packageFolderURL: URL = urlEnumeration.nextElement
      result.addAll(listUnder(packageName, packageFolderURL))
    }
    result
  }

  private def listUnder(packageName: String, packageFolderURL: URL): java.util.Collection[JavaFileObject] = {
    val directory: File = new File(packageFolderURL.getFile)
    if (directory.isDirectory) {
      // browse local .class files - useful for local execution
      processDir(packageName, directory)
    } else {
      // browse a jar file
      processJar(packageFolderURL)
    } // maybe there can be something else for more involved class loaders
  }

  private def processJar(packageFolderURL: URL): java.util.List[JavaFileObject] = {
    val result: java.util.List[JavaFileObject] = new java.util.ArrayList[JavaFileObject]
    try {
      val jarUriExternal: String = packageFolderURL.toExternalForm
      val jarUri: String = jarUriExternal.substring(0, jarUriExternal.lastIndexOf("!"))
      val jarConn: JarURLConnection = packageFolderURL.openConnection.asInstanceOf[JarURLConnection]
      val rootEntryName: String = jarConn.getEntryName
      val rootEnd: Int = rootEntryName.length + 1
      val entryEnum: java.util.Enumeration[JarEntry] = jarConn.getJarFile.entries
      while (entryEnum.hasMoreElements) {
        {
          val jarEntry: JarEntry = entryEnum.nextElement
          val name: String = jarEntry.getName
          if (name.startsWith(rootEntryName) && name.indexOf('/', rootEnd) == -1 && name.endsWith(PackageInternalsFinder.CLASS_FILE_EXTENSION)) {
            val uri: URI = URI.create(jarUri + "!/" + name)
            var binaryName: String = name.replaceAll("/", ".")
            binaryName = binaryName.replaceAll(PackageInternalsFinder.CLASS_FILE_EXTENSION + "$", "")
            result.add(new CustomJavaFileObject(binaryName, uri))
          }
        }
      }
    }
    catch {
      case e: Exception =>
        throw new RuntimeException("Unable to open " + packageFolderURL + " as a jar file", e)
    }
    result
  }

  private def processDir(packageName: String, directory: File): java.util.List[JavaFileObject] = {
    val result: java.util.List[JavaFileObject] = new java.util.ArrayList[JavaFileObject]
    val childFiles: Array[File] = directory.listFiles
    for (childFile <- childFiles) {
      if (childFile.isFile) {
        // We only want the .class files.
        if (childFile.getName.endsWith(PackageInternalsFinder.CLASS_FILE_EXTENSION)) {
          var binaryName: String = packageName + "." + childFile.getName
          binaryName = binaryName.replaceAll(PackageInternalsFinder.CLASS_FILE_EXTENSION + "$", "")
          result.add(new CustomJavaFileObject(binaryName, childFile.toURI))
        }
      }
    }
    result
  }
}
