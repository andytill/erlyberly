package erlyberly.node;

import com.ericsson.otp.erlang.*;

import java.io.IOException;

import static erlyberly.node.OtpUtil.atom;
import static erlyberly.node.OtpUtil.tuple;

public class NodeRPC {

    private static final OtpErlangAtom CALL_ATOM = atom("call");

    private static final OtpErlangAtom USER_ATOM = atom("user");

    private final OtpConn connection;

    private final OtpSelfNode self;

    private final int timeout;

    public NodeRPC(OtpSelfNode self, OtpConn connection) {
        this.connection = connection;
        this.self = self;
        this.timeout = 5000;
    }

    public NodeRPC(OtpSelfNode self, OtpConn connection, int timeoutMillis) {
        this.connection = connection;
        this.self = self;
        this.timeout = timeoutMillis;
    }

    public OtpErlangObject blockingRPC(OtpErlangAtom mod, OtpErlangAtom fun, OtpErlangList args) throws IOException, OtpErlangException {
        OtpMbox mbox = self.createMbox();
        try {
            sendRPC(mbox, mod, fun, args);
            return receiveResult(mbox);
        } finally {
            mbox.close();
        }
    }

    private OtpErlangObject receiveResult(OtpMbox mbox) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangObject message = mbox.receive(timeout);
        if (message == null)
            return null;
        if (!(message instanceof OtpErlangTuple))
            throw new RuntimeException("RPC response expected tuple but got " + message);
        OtpErlangTuple tupleMessage = (OtpErlangTuple) message;
        OtpErlangObject result = tupleMessage.elementAt(1);
        return result;
    }

    private void sendRPC(OtpMbox m, OtpErlangAtom mod, OtpErlangAtom fun, OtpErlangList args) throws IOException {
        OtpErlangTuple rpcMessage = tuple(m.self(), tuple(CALL_ATOM, mod, fun, args, USER_ATOM));

        connection.send(m.self(), "rex", rpcMessage);
    }
}
