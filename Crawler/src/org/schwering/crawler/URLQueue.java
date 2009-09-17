package org.schwering.crawler;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class URLQueue {
	private static final int QUEUE_SIZE = 8192;
	private static final long PUT_MILLIS = 500;
	private static final long TAKE_MILLIS = 10 * 1000;
	
	private BlockingQueue<DocumentURL> queue = 
		new LinkedBlockingQueue<DocumentURL>(QUEUE_SIZE);
	private Set<DocumentURL> set = 
		Collections.synchronizedSet(new HashSet<DocumentURL>());
	
	public URLQueue(Collection<DocumentURL> rootUrls) {
		if (rootUrls.size() > QUEUE_SIZE) {
			throw new IllegalArgumentException("Too many root URLs");
		}
		put(rootUrls);
	}
	
	public void put(DocumentURL url) {
		put(url, 0);
	}
	
	public void put(Collection<DocumentURL> urls) {
		for (DocumentURL url : urls) {
			put(url, PUT_MILLIS / urls.size() + 1);
		}
	}
	
	public void putOneOf(DocumentURL url, int total) {
		put(url, PUT_MILLIS / total + 1);
	}
	
	private void put(DocumentURL url, long millis) {
		if (!set.contains(url)) {
			try {
				if (queue.offer(url, millis, TimeUnit.MILLISECONDS)) {
					set.add(url);
				}
			} catch (InterruptedException exc) {
				Logger.error(getClass(), exc);
			}
		}
	}
	
	public DocumentURL take() {
		try {
			DocumentURL url = queue.poll(TAKE_MILLIS, TimeUnit.MILLISECONDS);
			if (url != null) {
				set.remove(url);
			}
			return url;
		} catch (InterruptedException exc) {
			Logger.error(getClass(), exc);
			return null;
		}
	}
	
	public int size() {
		return queue.size();
	}
}
