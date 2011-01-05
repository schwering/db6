package org.schwering.crawler;

import java.util.Collection;

import org.apache.log4j.Logger;

class CrawlerThread extends Thread {
	private static final Logger logger = Logger.getLogger(CrawlerThread.class);
	private final UrlQueue queue;
	private UrlFilter filter;
	private CrawlerThreadListener listener;

	public CrawlerThread(UrlQueue queue, UrlFilter filter,
			CrawlerThreadListener listener) {
		this.queue = queue;
		this.filter = filter;
		this.listener = listener;
	}

	public void run() {
		logger.debug("Thread started.");
		DocumentUrl url;
		while ((url = queue.take()) != null) {
			if (!filter.accept(url)) {
				continue;
			}
			DocumentReceiver docRec;
			try {
				docRec = new DocumentReceiver(url);
			} catch (DocumentException exc) {
				logger.debug("Failed: ("+ exc.getMessage() +")");
				continue;
			}
			/* Filter a second time, doc.getURL() might be different from url. */
			if (!filter.accept(docRec.getURL())) {
				continue;
			}
			listener.found(this, docRec.getURL());
			Collection<DocumentUrl> links = DocumentParser.extractLinks(docRec);
			for (DocumentUrl link : links) {
				if (filter.accept(link)) {
					queue.put(link);
					yield();
				}
			}
		}
		logger.error("Thread terminated.");
	}

	public void setFilter(UrlFilter filter) {
		this.filter = filter;
	}

	public void setListener(CrawlerThreadListener listener) {
		this.listener = listener;
	}
}
