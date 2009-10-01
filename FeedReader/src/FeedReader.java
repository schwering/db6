import java.net.URL;

import org.schwering.feed.Entry;
import org.schwering.feed.Feed;
import org.schwering.feed.FeedException;

public class FeedReader {

	public static void main(String[] args) {
		try {
			if (args.length == 0) {
				String defaultUrl = 
				"http://topthemenrss.sueddeutsche.de/pf/449042/http://rss.sueddeutsche.de/rss/Topthemen";
				//http://rss.cnn.com/rss/edition.rss";
				//http://www.nrw.de/rss-feeds/cat/0,3,174.xml";
				fetch(new URL(defaultUrl));
			} else {
				for (String arg : args) {
					fetch(new URL(arg));
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			System.out.println("ERROR: " + ex.getMessage());
		}
	}

	private static void fetch(URL url) throws FeedException {
		Feed feed = new Feed(url);
		System.out.println(feed.getURL());
		System.out.println(feed.getTitle());
		System.out.println(feed.getDescription());
		System.out.println(feed.getCategories());
		System.out.println(feed.getAuthor());
		for (Entry e : feed.getEntries()) {
			System.out.println();
			System.out.println(e);
		}
	}
	
	private static String compact(String s) {
		if (s.length() > 60) {
			s = s.substring(0, 50).trim() +"... (total: "+ s.length() +")";
		}
		return s;
	}
}