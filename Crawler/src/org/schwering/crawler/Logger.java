package org.schwering.crawler;

import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;

public final class Logger {
	private Logger() {
	}
	
	private static int sens = 3;
	private static PrintStream err = System.err;
	private static PrintStream out = System.out;
	
	public static void setSensitivity(int s) {
		sens = s;
	}
	
	public static int getSensitivity() {
		return sens;
	}
	
	public static void notify(Object o) {
		notify(null, o);
	}
	
	public static void error(Object o) {
		error(null, o);
	}
	
	public static void fatal(Object o) {
		fatal(null, o);
	}
	
	public static synchronized void notify(Object src, Object o) {
		if (sens >= 3)
			out.println(toString(src) + toString(o));
	}
	
	public static synchronized void error(Object src, Object o) {
		if (sens >= 2)
			err.println(toString(src) + toString(o));
	}
	
	public static synchronized void fatal(Object src, Object o) {
		if (sens >= 1)
			err.println(toString(src) + toString(o));
	}
	
	private static String toString(Object o) {
		if (o instanceof Class<?>) {
			return "["+ ((Class<?>)o).getCanonicalName() +"] ";
		} else if (o instanceof Thread) {
			return "["+ ((Thread)o).getName() +"] ";
		} else if (o instanceof Throwable) {
			Throwable t = (Throwable)o;
			StringWriter sw = new StringWriter();
			PrintWriter pw = new PrintWriter(sw);
			t.printStackTrace(pw);
			return sw.toString();
		} else {
			return o.toString();
		}
	}
}
