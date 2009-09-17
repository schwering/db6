package org.schwering.crawler;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class URLHistoryFilter implements URLFilter {
	private static class Counter {
		int cnt = 0;
	}
	
	private Set<DocumentURL> set = 
		Collections.synchronizedSet(new HashSet<DocumentURL>());
	private Map<DocumentURL, Counter> map = 
		Collections.synchronizedMap(new HashMap<DocumentURL, Counter>());
	
	public URLHistoryFilter(Crawler crawler) {
		crawler.addListener(new CrawlerListener() {
			@Override
			public void finished() {
			}

			@Override
			public void followed(DocumentURL url, 
					Collection<DocumentURL> links) {
			}

			@Override
			public void found(DocumentURL url) {
				set.add(url);
			}
		});
	}
	
	public Set<DocumentURL> getHistory() {
		return Collections.unmodifiableSet(set);
	}
	
	@Override
	public boolean accept(DocumentURL url) {
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
