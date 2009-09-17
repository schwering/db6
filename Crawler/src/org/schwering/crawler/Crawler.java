package org.schwering.crawler;

import java.util.Collection;
import java.util.LinkedList;

public class Crawler {
	static final String PROGRAM_NAME = "Dingsbot";
	static final String PROGRAM_VERSION = "0.1";
	static final String USER_AGENT = "Mozilla/5.0 (compatible; " +
			PROGRAM_NAME +"/"+ PROGRAM_VERSION +")";
	
	private Collection<DocumentURL> rootUrls;
	private int threadCount;
	private URLFilter filter;
	private CrawlerThreadListener threadListener = 
		new CrawlerThreadListenerImpl();
	private Collection<CrawlerListener> listeners =
		new LinkedList<CrawlerListener>();
	
	public Crawler(Collection<DocumentURL> rootUrls) {
		this(rootUrls, rootUrls.size());
	}
	
	public Crawler(Collection<DocumentURL> rootUrls, int threadCount) {
		this.rootUrls = rootUrls;
		this.threadCount = threadCount;
	}
	
	public void addListener(CrawlerListener listener) {
		listeners.add(listener);
	}
	
	public void removeListener(CrawlerListener listener) {
		listeners.remove(listener);
	}
	
	public void setFilter(URLFilter filter) {
		this.filter = filter;
	}
	
	public void setThreadCount(int threadCount) {
		this.threadCount = threadCount;
	}
	
	public void start() {
		URLQueue queue = new URLQueue(rootUrls);
		for (int i = 0; i < threadCount; i++) {
			CrawlerThread thread = new CrawlerThread(queue, filter, 
					threadListener);
			thread.start();
		}
	}
	
	private class CrawlerThreadListenerImpl 
	implements CrawlerThreadListener {
		@Override
		public void found(CrawlerThread thread, DocumentURL url) {
			for (CrawlerListener listener : listeners) {
				listener.found(url);
			}
		}
		
		@Override
		public void followed(CrawlerThread thread, DocumentURL url, 
				Collection<DocumentURL> links) {
			for (CrawlerListener listener : listeners) {
				listener.followed(url, links);
			}
		}
	}
}
