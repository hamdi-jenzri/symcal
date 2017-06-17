package com.github.symcal;

import java.io.*;
import java.util.*;
import javax.tools.*;
import java.net.URI;
import javax.tools.JavaFileObject;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.Modifier;

import java.util.List;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Collection;
import java.util.jar.JarEntry;
import java.io.IOException;
import java.io.File;
import java.net.URL;
import java.net.JarURLConnection;

/**
 * @author atamur
 * see http://atamur.blogspot.com/2009/10/using-built-in-javacompiler-with-custom.html
 * @since 15-Oct-2009
 */
public class CustomClassloaderJavaFileManager implements JavaFileManager {
    private final ClassLoader classLoader;
    private final StandardJavaFileManager standardFileManager;
    private final com.github.symcal.javacompiler.PackageInternalsFinder finder;

    public CustomClassloaderJavaFileManager(ClassLoader classLoader, StandardJavaFileManager standardFileManager) {
        this.classLoader = classLoader;
        this.standardFileManager = standardFileManager;
        finder = new com.github.symcal.javacompiler.PackageInternalsFinder(classLoader);
    }

    @Override
    public ClassLoader getClassLoader(Location location) {
        return classLoader;
    }

    @Override
    public String inferBinaryName(Location location, JavaFileObject file) {
        if (file instanceof com.github.symcal.javacompiler.CustomJavaFileObject) {
            return ((com.github.symcal.javacompiler.CustomJavaFileObject) file).binaryName();
        } else { // if it's not CustomJavaFileObject, then it's coming from standard file manager - let it handle the file
            return standardFileManager.inferBinaryName(location, file);
        }
    }

    @Override
    public boolean isSameFile(FileObject a, FileObject b) {
        return standardFileManager.isSameFile(a, b);
    }

    @Override
    public boolean handleOption(String current, Iterator<String> remaining) {
        return standardFileManager.handleOption(current, remaining);
    }

    @Override
    public boolean hasLocation(Location location) {
        return location == StandardLocation.CLASS_PATH || location == StandardLocation.PLATFORM_CLASS_PATH; // we don't care about source and other location types - not needed for compilation
    }

    @Override
    public JavaFileObject getJavaFileForInput(Location location, String className, JavaFileObject.Kind kind) throws IOException {
        return standardFileManager.getJavaFileForInput(location, className, kind);
    }

    @Override
    public JavaFileObject getJavaFileForOutput(Location location, String className, JavaFileObject.Kind kind, FileObject sibling) throws IOException {
        return standardFileManager.getJavaFileForOutput(location, className, kind, sibling);
    }

    @Override
    public FileObject getFileForInput(Location location, String packageName, String relativeName) throws IOException {
        return standardFileManager.getFileForInput(location, packageName, relativeName);
    }

    @Override
    public FileObject getFileForOutput(Location location, String packageName, String relativeName, FileObject sibling) throws IOException {
        return standardFileManager.getFileForOutput(location, packageName, relativeName, sibling);
    }

    @Override
    public void flush() throws IOException {
        // do nothing
    }

    @Override
    public void close() throws IOException {
        // do nothing
    }

    @Override
    public Iterable<JavaFileObject> list(Location location, String packageName, Set<JavaFileObject.Kind> kinds, boolean recurse) throws IOException {
        if (location == StandardLocation.PLATFORM_CLASS_PATH) { // let standard manager hanfle
            return standardFileManager.list(location, packageName, kinds, recurse);
        } else if (location == StandardLocation.CLASS_PATH && kinds.contains(JavaFileObject.Kind.CLASS)) {
            if (packageName.startsWith("java.") || packageName.startsWith("javax.")) { // a hack to let standard manager handle locations like "java.lang" or "java.util". Prob would make sense to join results of standard manager with those of my finder here
                return standardFileManager.list(location, packageName, kinds, recurse);
            } else { // app specific classes are here
                return finder.find(packageName);
            }
        }
        return Collections.emptyList();

    }

    @Override
    public int isSupportedOption(String option) {
        return -1;
    }

}

/**
 * @author atamur
 * @since 15-Oct-2009
 */
class CustomJavaFileObject implements JavaFileObject {
    private final String binaryName;
    private final URI uri;
    private final String name;

    public CustomJavaFileObject(String binaryName, URI uri) {
        this.uri = uri;
        this.binaryName = binaryName;
        name = uri.getPath() == null ? uri.getSchemeSpecificPart() : uri.getPath(); // for FS based URI the path is not null, for JAR URI the scheme specific part is not null
    }

    @Override
    public URI toUri() {
        return uri;
    }

    @Override
    public InputStream openInputStream() throws IOException {
        return uri.toURL().openStream(); // easy way to handle any URI!
    }

    @Override
    public OutputStream openOutputStream() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Reader openReader(boolean ignoreEncodingErrors) throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public Writer openWriter() throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public long getLastModified() {
        return 0;
    }

    @Override
    public boolean delete() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Kind getKind() {
        return Kind.CLASS;
    }

    @Override // copied from SImpleJavaFileManager
    public boolean isNameCompatible(String simpleName, Kind kind) {
        String baseName = simpleName + kind.extension;
        return kind.equals(getKind())
            && (baseName.equals(getName())
            || getName().endsWith("/" + baseName));
    }

    @Override
    public NestingKind getNestingKind() {
        throw new UnsupportedOperationException();
    }

    @Override
    public Modifier getAccessLevel() {
        throw new UnsupportedOperationException();
    }

    public String binaryName() {
        return binaryName;
    }


    @Override
    public String toString() {
        return "CustomJavaFileObject{" +
            "uri=" + uri +
            '}';
    }
}

/**
 * @author atamur
 * @since 15-Oct-2009
 */
class PackageInternalsFinder {
    private ClassLoader classLoader;
    private static final String CLASS_FILE_EXTENSION = ".class";

    public PackageInternalsFinder(ClassLoader classLoader) {
        this.classLoader = classLoader;
    }

    public List<JavaFileObject> find(String packageName) throws IOException {
        String javaPackageName = packageName.replaceAll("\\.", "/");

        List<JavaFileObject> result = new ArrayList<JavaFileObject>();

        Enumeration<URL> urlEnumeration = classLoader.getResources(javaPackageName);
        while (urlEnumeration.hasMoreElements()) { // one URL for each jar on the classpath that has the given package
            URL packageFolderURL = urlEnumeration.nextElement();
            result.addAll(listUnder(packageName, packageFolderURL));
        }

        return result;
    }

    private Collection<JavaFileObject> listUnder(String packageName, URL packageFolderURL) {
        File directory = new File(packageFolderURL.getFile());
        if (directory.isDirectory()) { // browse local .class files - useful for local execution
            return processDir(packageName, directory);
        } else { // browse a jar file
            return processJar(packageFolderURL);
        } // maybe there can be something else for more involved class loaders
    }

    private List<JavaFileObject> processJar(URL packageFolderURL) {
        List<JavaFileObject> result = new ArrayList<JavaFileObject>();
        try {
            String jarUriExternal = packageFolderURL.toExternalForm();
            String jarUri = jarUriExternal.substring(0, jarUriExternal.lastIndexOf("!"));

            JarURLConnection jarConn = (JarURLConnection) packageFolderURL.openConnection();
            String rootEntryName = jarConn.getEntryName();
            int rootEnd = rootEntryName.length() + 1;

            Enumeration<JarEntry> entryEnum = jarConn.getJarFile().entries();
            while (entryEnum.hasMoreElements()) {
                JarEntry jarEntry = entryEnum.nextElement();
                String name = jarEntry.getName();
                if (name.startsWith(rootEntryName) && name.indexOf('/', rootEnd) == -1 && name.endsWith(CLASS_FILE_EXTENSION)) {
                    URI uri = URI.create(jarUri + "!/" + name);
                    String binaryName = name.replaceAll("/", ".");
                    binaryName = binaryName.replaceAll(CLASS_FILE_EXTENSION + "$", "");

                    result.add(new com.github.symcal.javacompiler.CustomJavaFileObject(binaryName, uri));
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("Wasn't able to open " + packageFolderURL + " as a jar file", e);
        }
        return result;
    }

    private List<JavaFileObject> processDir(String packageName, File directory) {
        List<JavaFileObject> result = new ArrayList<JavaFileObject>();

        File[] childFiles = directory.listFiles();
        for (File childFile : childFiles) {
            if (childFile.isFile()) {
                // We only want the .class files.
                if (childFile.getName().endsWith(CLASS_FILE_EXTENSION)) {
                    String binaryName = packageName + "." + childFile.getName();
                    binaryName = binaryName.replaceAll(CLASS_FILE_EXTENSION + "$", "");

                    result.add(new com.github.symcal.javacompiler.CustomJavaFileObject(binaryName, childFile.toURI()));
                }
            }
        }

        return result;
    }
}
