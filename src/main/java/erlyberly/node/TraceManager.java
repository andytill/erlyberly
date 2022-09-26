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
package erlyberly.node;

import com.ericsson.otp.erlang.*;
import erlyberly.TraceLog;

import java.util.*;

class TraceManager {

    private static final OtpErlangAtom RETURN_FROM_ATOM = new OtpErlangAtom("return_from");

    private static final OtpErlangAtom EXCEPTION_FROM_ATOM = new OtpErlangAtom("exception_from");

    private static final OtpErlangAtom CALL_ATOM = new OtpErlangAtom("call");

    private final HashMap<String, Stack<TraceLog>> unfinishedCalls = new HashMap<>();


    List<TraceLog> collateTraces(final OtpErlangList traceLogs) {
        final List<TraceLog> traceList = new ArrayList<>();

        for (final OtpErlangObject obj : traceLogs) {
            this.decodeTraceLog(obj, traceList);
        }

        return traceList;
    }

    List<TraceLog> collateTraceSingle(final OtpErlangTuple traceLog) {
        final List<TraceLog> traceList = new ArrayList<>();
        this.decodeTraceLog(traceLog, traceList);
        return traceList;
    }

    private void decodeTraceLog(final OtpErlangObject otpErlangObject, final List<TraceLog> traceList) {
        final OtpErlangTuple tup = (OtpErlangTuple) otpErlangObject;
        final OtpErlangAtom traceType = (OtpErlangAtom) tup.elementAt(0);

        if (CALL_ATOM.equals(traceType)) {
            final TraceLog trace = TraceManager.proplistToTraceLog(tup);

            Stack<TraceLog> stack = this.unfinishedCalls.get(trace.getPidString());

            if (null == stack)
                stack = new Stack<>();

            stack.add(trace);

            this.unfinishedCalls.put(trace.getPidString(), stack);

            traceList.add(trace);
        } else if (RETURN_FROM_ATOM.equals(traceType) || EXCEPTION_FROM_ATOM.equals(traceType)) {
            final Map<Object, Object> map = TraceManager.propsFromTrace(tup);

            final Object object = map.get(TraceLog.ATOM_PID);

            if (null != object) {
                final OtpErlangString pidString = (OtpErlangString) object;

                final Stack<TraceLog> stack = this.unfinishedCalls.get(pidString.stringValue());
                if (null == stack)
                    return;

                final TraceLog traceLog = stack.pop();
                traceLog.complete(map);

                if (stack.isEmpty())
                    this.unfinishedCalls.remove(pidString.stringValue());
            }

        }
    }

    private static TraceLog proplistToTraceLog(final OtpErlangTuple tup) {
        final Map<Object, Object> map = TraceManager.propsFromTrace(tup);

        return new TraceLog(map);
    }

    private static Map<Object, Object> propsFromTrace(final OtpErlangTuple tup) {
        return OtpUtil.propsToMap((OtpErlangList) tup.elementAt(1));
    }
}
