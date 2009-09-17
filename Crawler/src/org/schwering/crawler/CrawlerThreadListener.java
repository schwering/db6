package org.schwering.crawler;

import java.util.Collection;

interface CrawlerThreadListener {
	void found(CrawlerThread thread, DocumentURL url);
	void followed(CrawlerThread thread, DocumentURL url, 
			Collection<DocumentURL> links);
}
