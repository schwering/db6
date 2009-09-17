package org.schwering.crawler;

import java.util.Collection;

public interface CrawlerListener {
	void found(DocumentURL url);
	void followed(DocumentURL url, Collection<DocumentURL> links);
	void finished();
}
