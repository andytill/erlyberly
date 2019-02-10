/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import erlyberly.node.OtpUtil;

public class CrashReport {

    private static final OtpErlangAtom ATOM_INITIAL_CALL = OtpUtil.atom("initial_call");

    private static final OtpErlangAtom ATOM_PID = OtpUtil.atom("pid");

    private static final OtpErlangAtom ATOM_REGISTERED_NAME = OtpUtil.atom("registered_name");

    private static final OtpErlangAtom ATOM_ERROR_INFO = OtpUtil.atom("error_info");

    private static final OtpErlangAtom ATOM_FILE = OtpUtil.atom("file");

    private static final OtpErlangAtom ATOM_LINE = OtpUtil.atom("line");

    private final Map<OtpErlangObject, OtpErlangObject> crashProps;

    private final String registeredName;

    private final OtpErlangObject terms;

    private final OtpErlangTuple errorInfo;

    private final LocalDateTime date = LocalDateTime.now();

    public CrashReport(OtpErlangObject obj) {
        this.terms = obj;
        crashProps  = OtpUtil.propsToMap((OtpErlangList) obj);
        errorInfo = (OtpErlangTuple) crashProps.get(ATOM_ERROR_INFO);

        // pull out the register name of the crashing process
        Object aRegName = crashProps.get(ATOM_REGISTERED_NAME);
        if(OtpUtil.list().equals(aRegName)) {
            registeredName = "";
        }
        else {
            registeredName = aRegName.toString();
        }
    }

    <T> List<T> mapStackTraces(StackTraceFn<T> fn) {
        OtpErlangList stackTrace = (OtpErlangList) errorInfo.elementAt(2);
        ArrayList<T> result = new ArrayList<T>();

        for (OtpErlangObject obj : stackTrace) {
            OtpErlangTuple tuple = (OtpErlangTuple) obj;
            OtpErlangAtom module = (OtpErlangAtom) tuple.elementAt(0);
            OtpErlangAtom function = (OtpErlangAtom) tuple.elementAt(1);
            OtpErlangObject argsOrArity = tuple.elementAt(2);
            OtpErlangLong arity;
            if(argsOrArity instanceof OtpErlangLong) {
                arity = (OtpErlangLong) argsOrArity;
            }
            else {
                OtpErlangList list = (OtpErlangList) argsOrArity;
                arity = new OtpErlangLong(list.arity());
            }
            Map<OtpErlangObject, OtpErlangObject> fileLineProps = OtpUtil.propsToMap((OtpErlangList) tuple.elementAt(3));
            OtpErlangString file = (OtpErlangString) fileLineProps.get(ATOM_FILE);
            OtpErlangLong line = (OtpErlangLong) fileLineProps.get(ATOM_LINE);

            String fileString = "";
            if(file != null) {
                fileString = file.stringValue();
            }
            result.add(
                fn.invoke(module, function, arity, fileString, line)
            );
        }
        return result;
    }

    public interface StackTraceFn<T> {
        T invoke(OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity, String file, OtpErlangLong line);
    }

    public OtpErlangPid getPid() {
        return (OtpErlangPid) crashProps.get(ATOM_PID);
    }

    public String getRegisteredName() {
        return registeredName;
    }

    public Object getProcessInitialCall() {
        return crashProps.get(ATOM_INITIAL_CALL);
    }

    public OtpErlangObject getProps() {
        return terms;
    }

    public OtpErlangAtom getErrorClass() {
        return (OtpErlangAtom) errorInfo.elementAt(0);
    }

    public OtpErlangObject getErrorReason() {
        return errorInfo.elementAt(1);
    }

    public LocalDateTime getDateTime() {
        return date;
    }

    public Optional<OtpErlangList> getCallArgs() {
        OtpErlangTuple staceTrace1 = (OtpErlangTuple) ((OtpErlangList) errorInfo.elementAt(2)).getHead();
        OtpErlangObject argsOrArity = staceTrace1.elementAt(2);
        if(argsOrArity instanceof OtpErlangList)
            return Optional.of((OtpErlangList)argsOrArity);
        else
            return Optional.empty();
    }
}
