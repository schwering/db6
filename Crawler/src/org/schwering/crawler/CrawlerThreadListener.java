package org.schwering.crawler;

import java.util.Collection;

interface CrawlerThreadListener {
	void found(CrawlerThread thread, DocumentUrl url);
	void followed(CrawlerThread thread, DocumentUrl url, 
			Collection<DocumentUrl> links);
}
