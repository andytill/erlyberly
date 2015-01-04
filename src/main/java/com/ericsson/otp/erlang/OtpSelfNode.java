package com.ericsson.otp.erlang;

import java.io.IOException;

public class OtpSelfNode extends OtpNode {

	public OtpSelfNode(String node, String cookie) throws IOException {
		super(node, cookie);
	}

	public OtpSelfNode(String node, String cookie, int port) throws IOException {
		super(node, cookie, port);
	}

	public OtpSelfNode(String node) throws IOException {
		super(node);
	}

	public OtpConn connect(OtpPeer aPeer) throws OtpAuthException, IOException {
		return new OtpConn(this, aPeer);
	}
}
