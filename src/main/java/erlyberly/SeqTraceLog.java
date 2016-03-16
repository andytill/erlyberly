package erlyberly;

import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

import erlyberly.node.OtpUtil;

public class SeqTraceLog {

    private final Object msgType;
    private final Object serial;
    private final String from;
    private final String to;
    private final Object message;
    private final Object timestamp;

    public SeqTraceLog(Object msg_type, Object serial, Object from, Object to, Object message, Object timestamp) {
        this.msgType = msg_type;
        this.serial = serial;
        this.from = stringValue(from);
        this.to = stringValue(to);
        this.message = message;
        this.timestamp = timestamp;
    }

    public static SeqTraceLog build(HashMap<Object, Object> props) {
        Object msg_type = props.get(OtpUtil.atom("msg_type"));
        Object serial = props.get(OtpUtil.atom("serial"));
        Object from = props.get(OtpUtil.atom("from"));
        Object to = props.get(OtpUtil.atom("to"));
        Object message = props.get(OtpUtil.atom("message"));
        Object timestamp = props.get(OtpUtil.atom("timestamp"));
        return new SeqTraceLog(msg_type, serial,  from, to, message, timestamp);
    }
    
    private String stringValue(Object obj) {
        if(obj instanceof OtpErlangString)
            return ((OtpErlangString) obj).stringValue();
        return obj.toString();
    }

    @Override
    public String toString() {
        return "SeqTraceLog [msg_type=" + msgType + ", serial=" + serial
                + ", from=" + from + ", to=" + to + ", message=" + message
                + "]";
    }

    public OtpErlangObject getMessage() {
        return (OtpErlangObject) message;
    }

    public Object getMsgType() {
        return msgType;
    }

    public Object getSerial() {
        return serial;
    }

    public Object getFrom() {
        return from;
    }

    public Object getTo() {
        return to;
    }

    public Object getTimestamp() {
        return timestamp;
    }
}
