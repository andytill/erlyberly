/**
 * erlyberly, erlang trace debugger
 * Copyright (C) 2016 Andy Till
 * <p>
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * <p>
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * <p>
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package erlyberly;

import com.ericsson.otp.erlang.*;
import erlyberly.node.OtpUtil;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class CrashReport {

    private static final OtpErlangAtom ATOM_INITIAL_CALL = OtpUtil.atom("initial_call");

    private static final OtpErlangAtom ATOM_PID = OtpUtil.atom("pid");

    private static final OtpErlangAtom ATOM_REGISTERED_NAME = OtpUtil.atom("registered_name");

    private static final OtpErlangAtom ATOM_ERROR_INFO = OtpUtil.atom("error_info");

    private static final OtpErlangAtom ATOM_FILE = OtpUtil.atom("file");

    private static final OtpErlangAtom ATOM_LINE = OtpUtil.atom("line");

    private final Map<Object, Object> crashProps;

    private final String registeredName;

    private final OtpErlangObject terms;

    private final OtpErlangTuple errorInfo;

    private final LocalDateTime date = LocalDateTime.now();

    public CrashReport(final OtpErlangObject obj) {
        super();
        this.terms = obj;
        this.crashProps = OtpUtil.propsToMap((OtpErlangList) obj);
        this.errorInfo = (OtpErlangTuple) this.crashProps.get(ATOM_ERROR_INFO);

        // pull out the register name of the crashing process
        final Object aRegName = this.crashProps.get(ATOM_REGISTERED_NAME);
        if (OtpUtil.list().equals(aRegName)) {
            this.registeredName = "";
        } else {
            this.registeredName = aRegName.toString();
        }
    }

    <T> List<T> mapStackTraces(final StackTraceFn<T> fn) {
        final OtpErlangList stackTrace = (OtpErlangList) this.errorInfo.elementAt(2);
        final List<T> result = new ArrayList<>();

        for (final OtpErlangObject obj : stackTrace) {
            final OtpErlangTuple tuple = (OtpErlangTuple) obj;
            final OtpErlangAtom module = (OtpErlangAtom) tuple.elementAt(0);
            final OtpErlangAtom function = (OtpErlangAtom) tuple.elementAt(1);
            final OtpErlangObject argsOrArity = tuple.elementAt(2);
            final OtpErlangLong arity;
            if (argsOrArity instanceof OtpErlangLong) {
                arity = (OtpErlangLong) argsOrArity;
            } else {
                final OtpErlangList list = (OtpErlangList) argsOrArity;
                arity = new OtpErlangLong(list.arity());
            }
            final Map<Object, Object> fileLineProps = OtpUtil.propsToMap((OtpErlangList) tuple.elementAt(3));
            final OtpErlangString file = (OtpErlangString) fileLineProps.get(ATOM_FILE);
            final OtpErlangLong line = (OtpErlangLong) fileLineProps.get(ATOM_LINE);

            String fileString = "";
            if (null != file) {
                fileString = file.stringValue();
            }
            result.add(fn.invoke(module, function, arity, fileString, line));
        }
        return result;
    }

    @FunctionalInterface
    public interface StackTraceFn<T> {
        T invoke(OtpErlangAtom module, OtpErlangAtom function, OtpErlangLong arity, String file, OtpErlangLong line);
    }

    public OtpErlangPid getPid() {
        return (OtpErlangPid) this.crashProps.get(ATOM_PID);
    }

    public String getRegisteredName() {
        return this.registeredName;
    }

    public Object getProcessInitialCall() {
        return this.crashProps.get(ATOM_INITIAL_CALL);
    }

    public OtpErlangObject getProps() {
        return this.terms;
    }

    public OtpErlangAtom getErrorClass() {
        return (OtpErlangAtom) this.errorInfo.elementAt(0);
    }

    public OtpErlangObject getErrorReason() {
        return this.errorInfo.elementAt(1);
    }

    public LocalDateTime getDateTime() {
        return this.date;
    }

    public Optional<OtpErlangList> getCallArgs() {
        final OtpErlangTuple staceTrace1 = (OtpErlangTuple) ((OtpErlangList) this.errorInfo.elementAt(2)).getHead();
        final OtpErlangObject argsOrArity = staceTrace1.elementAt(2);
        if (argsOrArity instanceof OtpErlangList) return Optional.of((OtpErlangList) argsOrArity);
        else return Optional.empty();
    }
}
