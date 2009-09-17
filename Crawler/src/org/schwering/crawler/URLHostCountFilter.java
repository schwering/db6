package org.schwering.crawler;

import java.util.HashSet;
import java.util.Set;

public class URLHostCountFilter implements URLFilter {
	private Set<String> hosts = new HashSet<String>();
	private int limit;
	
	public URLHostCountFilter(int limit) {
		this.limit = limit;
	}
	
	@Override
	public boolean accept(DocumentURL url) {
//		String host = url.getHost();
		return true;//pm.matches(url.getHost());
	}
}
