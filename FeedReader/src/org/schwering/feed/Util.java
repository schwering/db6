package org.schwering.feed;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

class Util {
	public static String toString(Object o) {
		Class<?> c = o.getClass();
		StringBuffer sb = new StringBuffer();
		Method[] methods = c.getMethods();
		for (int i = 0; i < methods.length; i++) {
			try {
				Method m = methods[i];
				String mn = m.getName();
				if (mn.startsWith("get") && m.getParameterTypes().length == 0) {
					Object r = methods[i].invoke(o);
					String s = (r == null) ? "null" : r.toString();
					sb.append(mn +"() = '"+ s +"'");
					if (i+1 < methods.length) {
						sb.append('\n');
					}
				}
			} catch (InvocationTargetException exc) {
				throw new RuntimeException(exc);
			} catch (IllegalAccessException exc) {
				throw new RuntimeException(exc);
			}
		}
		return sb.toString();
	}
}
