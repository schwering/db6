package org.schwering.crawler;

import java.util.Collection;
import java.util.LinkedList;

public class Crawler {
	static final String PROGRAM_NAME = "Dingsbot";
	static final String PROGRAM_VERSION = "0.1";
	static final String USER_AGENT = "Mozilla/5.0 (compatible; " +
			PROGRAM_NAME +"/"+ PROGRAM_VERSION +")";

	private final Collection<DocumentUrl> rootUrls;
	private int threadCount;
	private UrlFilter filter;
	private final CrawlerThreadListener threadListener =
		new CrawlerThreadListenerImpl();
	private final Collection<CrawlerListener> listeners =
		new LinkedList<CrawlerListener>();

	public Crawler(Collection<DocumentUrl> rootUrls) {
		this(rootUrls, rootUrls.size());
	}

	public Crawler(Collection<DocumentUrl> rootUrls, int threadCount) {
		this.rootUrls = rootUrls;
		this.threadCount = threadCount;
	}

	public void addListener(CrawlerListener listener) {
		listeners.add(listener);
	}

	public void removeListener(CrawlerListener listener) {
		listeners.remove(listener);
	}

	public void setFilter(UrlFilter filter) {
		this.filter = filter;
	}

	public void setThreadCount(int threadCount) {
		this.threadCount = threadCount;
	}

	public void start() {
		UrlQueue queue = new UrlQueue(rootUrls);
		for (int i = 0; i < threadCount; i++) {
			CrawlerThread thread = new CrawlerThread(queue, filter,
					threadListener);
			thread.start();
		}
	}

	private class CrawlerThreadListenerImpl implements CrawlerThreadListener {
		@Override
		public void found(CrawlerThread thread, DocumentUrl url) {
			for (CrawlerListener listener : listeners) {
				listener.found(url);
			}
		}

		@Override
		public void followed(CrawlerThread thread, DocumentUrl url,
				Collection<DocumentUrl> links) {
			for (CrawlerListener listener : listeners) {
				listener.followed(url, links);
			}
		}
	}
}
