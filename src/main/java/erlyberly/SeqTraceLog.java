package erlyberly;

import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangObject;

import erlyberly.node.OtpUtil;

public class SeqTraceLog {

	private Object msg_type;
	private Object serial;
	private Object from;
	private Object to;
	private Object message;

	public SeqTraceLog(Object msg_type, Object serial, Object from, Object to, Object message) {
		this.msg_type = msg_type;
		this.serial = serial;
		this.from = from;
		this.to = to;
		this.message = message;
	}

	public static SeqTraceLog build(HashMap<Object, Object> props) {
		Object msg_type = props.get(OtpUtil.atom("msg_type"));
		Object serial = props.get(OtpUtil.atom("serial"));
		Object from = props.get(OtpUtil.atom("from"));
		Object to = props.get(OtpUtil.atom("to"));
		Object message = props.get(OtpUtil.atom("message"));
		return new SeqTraceLog(msg_type, serial,  from, to, message);
	}

	@Override
	public String toString() {
		return "SeqTraceLog [msg_type=" + msg_type + ", serial=" + serial
				+ ", from=" + from + ", to=" + to + ", message=" + message
				+ "]";
	}

	public OtpErlangObject getMessage() {
		return (OtpErlangObject) message;
	}
}
