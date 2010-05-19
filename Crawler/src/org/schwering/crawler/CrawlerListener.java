package org.schwering.crawler;

import java.util.Collection;

public interface CrawlerListener {
	void found(DocumentUrl url);
	void followed(DocumentUrl url, Collection<DocumentUrl> links);
	void finished();
}
