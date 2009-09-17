package org.schwering.crawler;

import java.util.Collection;

class CrawlerThread extends Thread {
	private URLQueue queue;
	private URLFilter filter;
	private CrawlerThreadListener listener;
	
	public CrawlerThread(URLQueue queue, URLFilter filter, 
			CrawlerThreadListener listener) {
		this.queue = queue;
		this.filter = filter;
		this.listener = listener;
	}
	
	public void run() {
		Logger.notify(this, "Thread started.");
		DocumentURL url;
		while ((url = queue.take()) != null) {
			if (!filter.accept(url)) {
				continue;
			}
			DocumentReceiver docRec;
			try {
				docRec = new DocumentReceiver(url);
			} catch (DocumentException exc) {
				Logger.notify(this, "Failed: ("+ exc.getMessage() +")");
				continue;
			}
			/* Filter a second time, doc.getURL() might be different from url. */
			if (!filter.accept(docRec.getURL())) {
				continue;
			}
			listener.found(this, docRec.getURL());
			Collection<DocumentURL> links = DocumentParser.extractLinks(docRec);
			for (DocumentURL link : links) {
				if (filter.accept(link)) {
					queue.put(link);
					yield();
				}
			}
		}
		Logger.error(this, "Thread terminated.");
	}
	
	public void setFilter(URLFilter filter) {
		this.filter = filter;
	}
	
	public void setListener(CrawlerThreadListener listener) {
		this.listener = listener;
	}
}
