package org.schwering.feed;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;

import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndEntry;

public class Entry {
	private URL url;
	private SyndEntry e;
	private SyndContent c;
	
	public Entry(SyndEntry e) throws EntryException {
		this.e = e;
		this.c = e.getDescription();
		try {
			this.url = new URL(e.getUri());
		} catch (MalformedURLException exc) {
			throw new EntryException("Invalid entry URL", exc);
		}
	}
	
	public URL getURL() {
		return url;
	}
	
	public String getTitle() {
		return e.getTitle();
	}
	
	public Date getDate() {
		return e.getPublishedDate();
	}
	
	public Date getUpdateDate() {
		return e.getUpdatedDate();
	}
	
	public String getContentType() {
		return c.getType() == null ? "text/plain" : c.getType();
	}
	
	public String getContent() {
		return c.getValue();
	}
	
	@Override
	public String toString() {
		return Util.toString(this);
	}
}
