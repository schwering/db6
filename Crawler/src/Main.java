import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.BasicConfigurator;
import org.schwering.crawler.*;

public class Main {
	private final String[] rootStrings = new String[] {
		"http://schwering.ath.cx/",
		"http://www.cnn.com",
		"http://de.wikipedia.org",
		"http://en.wikipedia.org",
		"http://www.faz.net",
		"http://www.microsoft.com",
		"http://www.google.com",
		"http://www.sun.com",
		"http://www.whitehouse.gov",
		"http://www.bundestag.de",
		"http://www.bild.de",
		"http://www.heise.de",
		"http://www.twitter.de",
		"http://www.ebay.com",
//		"http://de.wikipedia.org/wiki/A",
//		"http://de.wikipedia.org/wiki/B",
//		"http://de.wikipedia.org/wiki/C",
//		"http://de.wikipedia.org/wiki/D",
//		"http://de.wikipedia.org/wiki/E",
//		"http://de.wikipedia.org/wiki/F",
//		"http://de.wikipedia.org/wiki/G",
//		"http://de.wikipedia.org/wiki/H",
//		"http://de.wikipedia.org/wiki/I",
//		"http://de.wikipedia.org/wiki/J",
//		"http://de.wikipedia.org/wiki/K",
//		"http://de.wikipedia.org/wiki/L",
//		"http://de.wikipedia.org/wiki/M",
//		"http://de.wikipedia.org/wiki/O",
//		"http://de.wikipedia.org/wiki/P",
//		"http://de.wikipedia.org/wiki/Q",
//		"http://de.wikipedia.org/wiki/R",
//		"http://de.wikipedia.org/wiki/S",
//		"http://de.wikipedia.org/wiki/T",
//		"http://de.wikipedia.org/wiki/U",
//		"http://de.wikipedia.org/wiki/V",
//		"http://de.wikipedia.org/wiki/W",
//		"http://de.wikipedia.org/wiki/X",
//		"http://de.wikipedia.org/wiki/Y",
//		"http://de.wikipedia.org/wiki/Z",
	};
	
	private int count = 0;
	
	public static void main(String[] args) {
		BasicConfigurator.configure();
		new Main();
	}
	
	private Main() {
		Collection<DocumentUrl> rootUrls = new LinkedList<DocumentUrl>();
		for (int i = 0; i < rootStrings.length; i++) {
			try {
				rootUrls.add(new DocumentUrl(rootStrings[i]));
			} catch (DocumentUrlException exc) {
				System.err.println("Invalid URL: "+ rootStrings[i]);
				exc.printStackTrace();
				return;
			}
		}
		
		Crawler crawler = new Crawler(rootUrls);
		
		UrlFilter fileTypeFilter = new UrlGuessedContentTypeFilter(new PatternMatcher("text/.*"));
//		URLFilter hostPatternFilter = new URLHostPatternFilter(new PatternMatcher("schwering.ath.cx"));
//		URLFilter hostPatternFilter = new URLHostPatternFilter(new PatternMatcher("de.wikipedia.org"));
//		URLFilter filePatternFilter = new URLFilePatternFilter(new PatternMatcher("(/wiki/.*)|"));
		UrlFilter historyFilter = new UrlHistoryFilter(crawler);
		UrlFilter filter = new UrlFilterConjunction(new UrlFilter[] {
//				hostPatternFilter, 
//				filePatternFilter,
				fileTypeFilter, 
				historyFilter });
		
		CrawlerListener listener = new CrawlerListener() {
			public void found(DocumentUrl url) {
				Thread t = Thread.currentThread();
				System.out.println("["+ t.getName() +"] "+
						url);
				synchronized (this) {
					count++;
				}
			}
			
			public void followed(DocumentUrl url, Collection<DocumentUrl> links) {
			}
			
			public void finished() {
				System.out.println("Finished, could start a new crawler.");
			}
		};
		crawler.setFilter(filter);
		crawler.addListener(listener);
		crawler.setThreadCount(100);
		crawler.start();
		supervise();
	}
	
	private void supervise() {
		final int SLEEP_TIME = 10;
		for (int i = 0; i < 3*60*60/SLEEP_TIME; i++){
			try {
				Thread.sleep(SLEEP_TIME * 1000);
				synchronized (this) {
					System.out.println(count +" pages in "+
							((i+1) * SLEEP_TIME) +" seconds.");
				}
			} catch (Exception exc) {
				exc.printStackTrace();
			}
		}
		System.exit(0);
	}
}
