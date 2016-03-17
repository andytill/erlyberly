package com.ericsson.otp.erlang;

import java.io.IOException;

public class OtpConn extends AbstractConnection {
    
    private final OtpSelfNode self2;

    protected OtpConn(OtpSelfNode self, OtpPeer other) 
            throws IOException, OtpAuthException 
    {
        super(self, other);
        
        self2 = self;
        
        // thread start
        start();
    }

    @Override
    public void deliver(final Exception e) {
        e.printStackTrace();
    }

    @Override
    public void deliver(OtpMsg msg) {
        self2.deliver(msg);
    }
    
    public void send(OtpErlangPid pid, OtpErlangPid dest, OtpErlangObject msg)
        throws IOException 
    {
        // encode and send the message
        super.sendBuf(pid, dest, new OtpOutputStream(msg));
    }
    
    public void send(OtpErlangPid pid, final String dest, final OtpErlangObject msg)
            throws IOException 
    {
        // encode and send the message
        super.sendBuf(pid, dest, new OtpOutputStream(msg));
    }
}
