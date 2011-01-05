package org.schwering.crawler;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.apache.log4j.Logger;

public class UrlQueue {
	private static final Logger logger = Logger.getLogger(UrlQueue.class);
	private static final int QUEUE_SIZE = 8192;
	private static final long PUT_MILLIS = 500;
	private static final long TAKE_MILLIS = 10 * 1000;

	private final BlockingQueue<DocumentUrl> queue =
		new LinkedBlockingQueue<DocumentUrl>(QUEUE_SIZE);
	private final Set<DocumentUrl> set =
		Collections.synchronizedSet(new HashSet<DocumentUrl>());

	public UrlQueue(Collection<DocumentUrl> rootUrls) {
		if (rootUrls.size() > QUEUE_SIZE) {
			throw new IllegalArgumentException("Too many root URLs");
		}
		put(rootUrls);
	}

	public void put(DocumentUrl url) {
		put(url, 0);
	}

	public void put(Collection<DocumentUrl> urls) {
		for (DocumentUrl url : urls) {
			put(url, PUT_MILLIS / urls.size() + 1);
		}
	}

	public void putOneOf(DocumentUrl url, int total) {
		put(url, PUT_MILLIS / total + 1);
	}

	private void put(DocumentUrl url, long millis) {
		if (!set.contains(url)) {
			try {
				if (queue.offer(url, millis, TimeUnit.MILLISECONDS)) {
					set.add(url);
				}
			} catch (InterruptedException exc) {
				logger.error(getClass(), exc);
			}
		}
	}

	public DocumentUrl take() {
		try {
			DocumentUrl url = queue.poll(TAKE_MILLIS, TimeUnit.MILLISECONDS);
			if (url != null) {
				set.remove(url);
			}
			return url;
		} catch (InterruptedException exc) {
			logger.error(getClass(), exc);
			return null;
		}
	}

	public int size() {
		return queue.size();
	}
}
