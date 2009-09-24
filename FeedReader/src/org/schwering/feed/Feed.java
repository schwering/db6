package org.schwering.feed;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;

public class Feed {
	private URL url;
	private SyndFeed feed;
	
	public Feed(URL url) throws org.schwering.feed.FeedException {
		this.url = url;
		SyndFeedInput input = new SyndFeedInput();
		try {
			this.feed = input.build(new XmlReader(url));
		} catch (FeedException exc) {
			throw new org.schwering.feed.FeedException("Could not create feed", exc);
		} catch (IOException exc) {
			throw new org.schwering.feed.FeedException("Could not create feed", exc);
		}
		if (this.feed == null) {
			throw new org.schwering.feed.FeedException("Could not create feed", new NullPointerException());
		}
	}
	
	public URL getURL() {
		return url;
	}
	
	public String getAuthor() {
		return feed.getAuthor();
	}
	
	public String getTitle() {
		return feed.getTitle();
	}
	
	public String getDescription() {
		return feed.getDescription();
	}
	
	@SuppressWarnings("unchecked")
	public List<String> getAuthors() {
		return (List<String>) feed.getAuthors();
	}
	
	@SuppressWarnings("unchecked")
	public List<String> getCategories() {
		return (List<String>) feed.getCategories();
	}
	
	@SuppressWarnings("unchecked")
	public List<Entry> getEntries() {
		List<SyndEntry> ses = (List<SyndEntry>) feed.getEntries();
		List<Entry> es = new ArrayList<Entry>(ses.size());
		for (SyndEntry se : ses) {
			try {
				es.add(new Entry(se));
			} catch (EntryException exc) {
				// nothing
			}
		}
		return es;
	}
	
	@Override
	public String toString() {
		return Util.toString(this);
	}
}
