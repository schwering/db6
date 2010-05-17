package org.schwering.crawler;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class UrlHistoryFilter implements UrlFilter {
	private static class Counter {
		int cnt = 0;
	}
	
	private final Set<DocumentUrl> set = 
		Collections.synchronizedSet(new HashSet<DocumentUrl>());
	private final Map<DocumentUrl, Counter> map = 
		Collections.synchronizedMap(new HashMap<DocumentUrl, Counter>());
	
	public UrlHistoryFilter(Crawler crawler) {
		crawler.addListener(new CrawlerListener() {
			@Override
			public void finished() {
			}

			@Override
			public void followed(DocumentUrl url, 
					Collection<DocumentUrl> links) {
			}

			@Override
			public void found(DocumentUrl url) {
				set.add(url);
			}
		});
	}
	
	public Set<DocumentUrl> getHistory() {
		return Collections.unmodifiableSet(set);
	}
	
	@Override
	public boolean accept(DocumentUrl url) {
		if (set.contains(url)) {
			return false;
		} else {
			Counter ctr = map.get(url);
			if (ctr == null) {
				ctr = new Counter();
				map.put(url, ctr);
			}
			ctr.cnt++;
			return ctr.cnt < 5;
		}
	}
}
