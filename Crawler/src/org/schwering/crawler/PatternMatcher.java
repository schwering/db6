package org.schwering.crawler;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;

public class PatternMatcher {
	private Set<String> allowed = new HashSet<String>();
	private List<Pattern> patterns = new LinkedList<Pattern>();
	
	public PatternMatcher(String regex) {
		addPattern(regex);
	}
	
	public PatternMatcher(Collection<String> regexes) {
		for (String regex : regexes) {
			addPattern(regex);
		}
	}
	
	public void addPattern(String regex) {
		if (!allowed.contains(regex)) {
			Pattern p = Pattern.compile(regex);
			allowed.add(regex);
			patterns.add(p);
		}
	}
	
	public boolean matches(String str) {
		for (Pattern p : patterns) {
			if (p.matcher(str).matches()) {
				return true;
			}
		}
		return false;
	}
}
