package erlyberly;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class CrashReport {
    
    private static final OtpErlangAtom ATOM_FILE = OtpUtil.atom("file");
    
    private static final OtpErlangAtom ATOM_LINE = OtpUtil.atom("line");
    
    private final HashMap<Object, Object> crashProps;

    public CrashReport(HashMap<Object, Object> hashMap) {
        crashProps = hashMap;
    }
    
    <T> List<T> mapStackTraces(StackTraceFn<T> fn) {
        OtpErlangTuple errorInfo = (OtpErlangTuple) crashProps.get(OtpUtil.atom("error_info"));
        OtpErlangList stackTrace = (OtpErlangList) errorInfo.elementAt(2);
        ArrayList<T> result = new ArrayList<T>();
        
        for (OtpErlangObject obj : stackTrace) {
            OtpErlangTuple tuple = (OtpErlangTuple) obj;
            OtpErlangAtom module = (OtpErlangAtom) tuple.elementAt(0);
            OtpErlangAtom function = (OtpErlangAtom) tuple.elementAt(1);
            OtpErlangLong arity = (OtpErlangLong) tuple.elementAt(2);
            HashMap<Object, Object> fileLineProps = OtpUtil.propsToMap((OtpErlangList) tuple.elementAt(3));
            OtpErlangString file = (OtpErlangString) fileLineProps.get(ATOM_FILE);
            OtpErlangLong line = (OtpErlangLong) fileLineProps.get(ATOM_LINE);
            
            result.add(
                fn.invoke(module, function, arity, file.stringValue(), line)
            );
        }
        return result;
    }
    
    public interface StackTraceFn<T> {
        T invoke(OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity, String file, OtpErlangLong line);
    }

}
