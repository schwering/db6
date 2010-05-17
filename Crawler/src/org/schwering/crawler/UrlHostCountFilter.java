package org.schwering.crawler;

//import java.util.HashSet;
//import java.util.Set;

public class UrlHostCountFilter implements UrlFilter {
//	private final Set<String> hosts = new HashSet<String>();
//	private final int limit;
	
	public UrlHostCountFilter(int limit) {
//		this.limit = limit;
	}
	
	@Override
	public boolean accept(DocumentUrl url) {
//		String host = url.getHost();
		return true;//pm.matches(url.getHost());
	}
}
