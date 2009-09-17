package org.schwering.crawler;

import java.io.IOException;
import java.io.InputStream;
import java.net.ContentHandler;
import java.net.URLConnection;

class DocumentReceiver {
	private static PatternMatcher ACCEPTED_CONTENT_TYPE_PATTERN = 
		new PatternMatcher("text/.*");
	
	static {
		ContentHandlerFactory fac = 
			new ContentHandlerFactory(ACCEPTED_CONTENT_TYPE_PATTERN);
		URLConnection.setContentHandlerFactory(fac);
	}
	
	private DocumentURL url;
	private String content;
	
	public DocumentReceiver(String url) throws DocumentException {
		this(new DocumentURL(url));
	}
	
	public DocumentReceiver(DocumentURL url) throws DocumentException {
		if (url == null) {
			throw new DocumentURLException();
		}
		URLConnection conn;
		try {
			conn = url.getURL().openConnection();
		} catch (IOException exc) {
			throw new DocumentConnectionException(exc);
		}
		conn.setConnectTimeout(3 * 1000);
		conn.setReadTimeout(1 * 1000);
		conn.setAllowUserInteraction(false);
		conn.setRequestProperty("User-Agent", Crawler.USER_AGENT);
		String contentType = getContentType(conn);
		if (!ACCEPTED_CONTENT_TYPE_PATTERN.matches(contentType)) {
			throw new DocumentException("URL "+ url +" invalid content type "+ 
					contentType);
		}
		Object content;
		try {
			content = conn.getContent();
		} catch (Exception exc) {
			throw new DocumentException("Loading URL "+ url +" failed", exc);
		}
		if (content == null) {
			throw new DocumentException("URL handler didn't handle "+ url);
		}
		this.url = new DocumentURL(conn.getURL());
		this.content = (String)content;
	}
	
	public DocumentURL getURL() {
		return url;
	}
	
	public String getContent() {
		return content;
	}

	@Override
	public boolean equals(Object obj) {
		return obj != null && obj instanceof DocumentReceiver ? 
				((DocumentReceiver)obj).url.equals(url) : false;
	}

	@Override
	public int hashCode() {
		return url.hashCode();
	}
	
	private static String getContentType(URLConnection conn) {
		String contentType = conn.getContentType();
		if (contentType == null) {
			return "unknown/unknown";
		}
		int i;
		if ((i = contentType.indexOf(';')) != -1) {
			return contentType.substring(0, i);
		} else {
			return contentType;
		}
	}
	
	private static class ContentHandlerFactory 
	implements java.net.ContentHandlerFactory {
		private static class TextContentHandler extends ContentHandler {
			@Override
			public Object getContent(URLConnection conn) throws IOException {
				InputStream is = conn.getInputStream();
				StringBuffer sb = new StringBuffer();
				byte[] buf = new byte[4096];
				int len;
				while ((len = is.read(buf)) != -1) {
					sb.append(new String(buf, 0, len));
				}
				return sb.toString();
			}
		}
		
		private static final ContentHandler HANDLER_INSTANCE =
			new TextContentHandler();
		
		private PatternMatcher contentTypePatterns;

		private ContentHandlerFactory(PatternMatcher contentTypePatterns) {
			this.contentTypePatterns = contentTypePatterns;
		}
		
		@Override
		public ContentHandler createContentHandler(String contentType) {
			if (contentType != null && contentTypePatterns.matches(contentType)) {
				return HANDLER_INSTANCE;
			} else {
				return null;
			}
		}
	}
}
