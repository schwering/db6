package org.schwering.crawler;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

class DocumentParser {
	public static final Collection<DocumentUrl> extractLinks(DocumentUrl url)
	throws DocumentException {
		return extractLinks(new DocumentReceiver(url));
	}
	
	static final Collection<DocumentUrl> extractLinks(DocumentReceiver doc) {
		String content = doc.getContent();
		Set<String> links = new HashSet<String>();
		int from = 0;
		int to = -1;
		try {
			while ((from = content.indexOf("href=", to+1)) != -1) {
				if (content.charAt(from + 5) == '\"') {
					from += 6;
					to = content.indexOf('\"', from);
				} else if (content.charAt(from + 5) == '\'') {
					from += 6;
					to = content.indexOf('\'', from);
				} else {
					from += 5;
					to = min(content.indexOf(' ', from), 
							content.indexOf('>', from));
				}
				if (to == -1) {
					to = from;
					continue;
				}
				String link = content.substring(from, to);
				links.add(link);
			}
		} catch (IndexOutOfBoundsException exc) {
			// nothing, might come from invalid documents
		}
		
		Set<DocumentUrl> urls = new HashSet<DocumentUrl>();
		for (String link : links) {
			try {
				DocumentUrl newUrl;
				if (DocumentUrl.isURI(link)) {
					newUrl = new DocumentUrl(link);
				} else {
					newUrl = new DocumentUrl(doc.getURL(), link);
				}
				urls.add(newUrl);
			} catch (Exception exc) {
				// could debug
			}
		}
		return urls;
	}
	
	private static int min(int i, int j) {
		return (i < j && i >= 0) ? i : j;
	}
}
