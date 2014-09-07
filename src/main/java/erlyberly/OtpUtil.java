package erlyberly;

import java.util.HashMap;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class OtpUtil {
	public static HashMap<Object, Object> propsToMap(OtpErlangList pinfo) {
		HashMap<Object, Object> map = new HashMap<>();
		for (OtpErlangObject otpErlangObject : pinfo) {
			if(otpErlangObject instanceof OtpErlangTuple && ((OtpErlangTuple) otpErlangObject).arity() == 2) {
				OtpErlangTuple tuple = ((OtpErlangTuple) otpErlangObject);
				map.put(tuple.elementAt(0), tuple.elementAt(1));
			}
		}
		return map;
	}
}
