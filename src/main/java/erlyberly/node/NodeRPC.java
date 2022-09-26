package erlyberly.node;

import com.ericsson.otp.erlang.*;

import java.io.IOException;

class NodeRPC {

    private static final OtpErlangAtom CALL_ATOM = OtpUtil.atom("call");

    private static final OtpErlangAtom USER_ATOM = OtpUtil.atom("user");

    private final OtpConn connection;

    private final OtpSelfNode self;

    private final int timeout;

    NodeRPC(final OtpSelfNode self, final OtpConn connection) {
        super();
        this.connection = connection;
        this.self = self;
        this.timeout = 5000;
    }

    NodeRPC(final OtpSelfNode self, final OtpConn connection, final int timeoutMillis) {
        super();
        this.connection = connection;
        this.self = self;
        this.timeout = timeoutMillis;
    }

    OtpErlangObject blockingRPC(final OtpErlangAtom mod, final OtpErlangAtom fun, final OtpErlangList args) throws IOException, OtpErlangException {
        final OtpMbox mbox = this.self.createMbox();
        try {
            this.sendRPC(mbox, mod, fun, args);
            return this.receiveResult(mbox);
        } finally {
            mbox.close();
        }
    }

    private OtpErlangObject receiveResult(final OtpMbox mbox) throws OtpErlangExit, OtpErlangDecodeException {
        final OtpErlangObject message = mbox.receive(this.timeout);
        if (null == message)
            return null;
        if (!(message instanceof OtpErlangTuple))
            throw new RuntimeException("RPC response expected tuple but got " + message);
        final OtpErlangTuple tupleMessage = (OtpErlangTuple) message;
        return tupleMessage.elementAt(1);
    }

    private void sendRPC(final OtpMbox m, final OtpErlangAtom mod, final OtpErlangAtom fun, final OtpErlangList args) throws IOException {
        final OtpErlangTuple rpcMessage = OtpUtil.tuple(m.self(), OtpUtil.tuple(CALL_ATOM, mod, fun, args, USER_ATOM));

        this.connection.send(m.self(), "rex", rpcMessage);
    }
}
