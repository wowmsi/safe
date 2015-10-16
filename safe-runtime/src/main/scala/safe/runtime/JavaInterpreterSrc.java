package safe.runtime;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.Arrays;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import javax.tools.JavaCompiler.CompilationTask;
import javax.tools.JavaFileObject.Kind;

public class JavaInterpreterSrc {

  private JavaInterpreterSrc classLoader;

  public static boolean compileSources(String className, String source) {
    String classOutputFolder = ".";

    JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
    DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<JavaFileObject>();

    JavaFileObject file = new JavaSourceFromString(className, source);

    Iterable<? extends JavaFileObject> compilationUnits = Arrays.asList(file);
    //specify classes output folder
    Iterable<java.lang.String> options = Arrays.asList("-d", classOutputFolder);
    CompilationTask task = compiler.getTask(null, null, diagnostics, null, null, compilationUnits);

    boolean success = task.call();

    if (!success)
        throw new Error("compilation failed");

    return success;
  }

  public static void eval(String className, String args[]) throws IOException {
      try {
        Class.forName("HelloWorld").getDeclaredMethod("main", new Class[] { String[].class })
            .invoke(null, new Object[] { null });
      } catch (ClassNotFoundException e) {
        System.err.println("Class not found: " + e);
      } catch (NoSuchMethodException e) {
        System.err.println("No such method: " + e);
      } catch (IllegalAccessException e) {
        System.err.println("Illegal access: " + e);
      } catch (InvocationTargetException e) {
        System.err.println("Invocation target: " + e);
      }
    }
}

class JavaSourceFromString extends SimpleJavaFileObject {
  final String code;

  JavaSourceFromString(String name, String code) {
    super(URI.create("string:///" + name.replace('.','/') + Kind.SOURCE.extension), Kind.SOURCE);
    this.code = code;
  }

  @Override
  public CharSequence getCharContent(boolean ignoreEncodingErrors) {
    return code;
  }
}
