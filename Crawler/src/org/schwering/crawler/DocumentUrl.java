package org.schwering.crawler;

import java.net.URISyntaxException;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

public class DocumentUrl {
	private static final Map<String, Integer> protocolToPort =
		new HashMap<String, Integer>();

	static {
		protocolToPort.put("http", 80);
	}

	private String protocol;
	private int port;
	private String host;
	private String file;
	private String[] domainLevels;
	private URI uri;

	public DocumentUrl(String url) throws DocumentUrlException {
		this(toURI(url));
	}

	public DocumentUrl(URL url) throws DocumentUrlException {
		this(toURI(url));
	}

	public DocumentUrl(URI uri) throws DocumentUrlException {
		this(uri.getScheme(), uri.getHost(), uri.getPort(), uri.getPath());
	}

	public DocumentUrl(DocumentUrl docUrl, String toFile) throws DocumentUrlException {
		this(docUrl.getURI().getScheme(),
				docUrl.getURI().getHost(),
				docUrl.getURI().getPort(),
				navigate(docUrl.getURI().getPath(), toFile));
	}

	private DocumentUrl(String protocol, String host, int port, String file)
	throws DocumentUrlException {
		this.protocol = normalizeProtocol(protocol);
		this.port = port != -1 ? port : protocolToPort.get(getProtocol());
		this.host = normalizeHost(host);
		this.file = normalizeFile(file);

		String[] hostTokens = getHost().split("\\.");
		this.domainLevels = new String[hostTokens.length];
		for (int i = 0; i < hostTokens.length; i++) {
			this.domainLevels[i] = hostTokens[hostTokens.length - 1 - i];
		}

		try {
			this.uri = new URI(getProtocol() +"://"+ getHost() +":"+ getPort() +
                                        "/"+ getFile());
		} catch (URISyntaxException exc) {
			throw new DocumentUrlException(exc);
		}
	}

	public int getDomainLevel() {
		return domainLevels.length;
	}

	public String getDomainId(int n) {
		StringBuffer sb = new StringBuffer();
		for (int i = 1; i <= n; i++) {
			sb.append(domainLevels[i-1]);
			if (i < n) {
				sb.append('.');
			}
		}
		return sb.toString();
	}

	public String getDomainId() {
		return getDomainId(getDomainLevel());
	}

	public String getProtocol() {
		return protocol;
	}

	public int getPort() {
		return port;
	}

	public String getHost() {
		return host;
	}

	public String getFile() {
		return file;
	}

	public URI getURI() {
		return uri;
	}

	@Override
	public boolean equals(Object obj) {
		return obj != null &&
			obj instanceof DocumentUrl &&
			uri.equals(((DocumentUrl)obj).getURI());
	}

	@Override
	public int hashCode() {
		return uri.hashCode();
	}

	@Override
	public String toString() {
		return getDomainId() +
			":"+ getProtocol() +
			":"+ getPort() +
			":"+ getFile();
	}

	public static boolean isURI(String uri) {
		try {
			toURI(uri);
			return true;
		} catch (DocumentUrlException exc) {
			return false;
		}
	}

	private static URI toURI(String uri) throws DocumentUrlException {
		try {
			return new URI(uri);
		} catch (URISyntaxException exc) {
			throw new DocumentUrlException(uri);
		}
	}

	private static URI toURI(URL uri) throws DocumentUrlException {
		try {
			return uri.toURI();
		} catch (URISyntaxException exc) {
			throw new DocumentUrlException(exc);
		}
	}

	private static String getParent(String path) {
		if (path.length() == 0 || path.equals("/")) {
			return "/";
		}
		int i = path.lastIndexOf("/", path.length() - 2);
		if (i == -1) {
			i = 0;
		}
		if (i+1 == path.length()) {
			return getParent(path.substring(0, i));
		} else {
			return path.substring(0, i+1);
		}
	}

	private static String navigate(String base, String app) {
		if (app.startsWith("/")) {
			return app;
		} else if (base.endsWith("/")) {
			return base + app;
		} else {
			return getParent(base) + app;
		}
	}

	private static String normalizeProtocol(String protocol) {
		return protocol.toLowerCase();
	}

	private static String normalizeHost(String host) {
		return host.toLowerCase();
	}

	private static String normalizeFile(String file) {
		if (file == null || file.length() == 0) {
			return "/";
		}

		String params = "";
		int i;
		if ((i = file.indexOf('?')) != -1) {
			params = file.substring(i);
			file = file.substring(0, i);
		}
		i = -1;
		while ((i = file.indexOf("/..", i+1)) != -1) {
			if (i+3 == file.length() || file.charAt(i+3) == '/') {
				int j;
				if (i == 0) {
					file = file.substring(3);
				} else if ((j = file.lastIndexOf('/', i-1)) != -1) {
						file = file.substring(0, j) + file.substring(i+3);
				}
			}
		}
		i = -1;
		while ((i = file.indexOf("/.", i+1)) != -1) {
			if (i+2 == file.length() || file.charAt(i+2) == '/') {
				file = file.substring(0, i) + file.substring(i+2);
			}
		}

		return file + params;
	}
}
