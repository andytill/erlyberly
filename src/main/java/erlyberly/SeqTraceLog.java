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

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import erlyberly.node.OtpUtil;

import java.util.Map;

public class SeqTraceLog {

    private final Object msgType;
    private final Object serial;
    private final String from;
    private final String to;
    private final Object message;
    private final Object timestamp;

    public SeqTraceLog(final Object msgType, final Object serial, final Object from, final Object to, final Object message, final Object timestamp) {
        super();
        this.msgType = msgType;
        this.serial = serial;
        this.from = SeqTraceLog.stringValue(from);
        this.to = SeqTraceLog.stringValue(to);
        this.message = message;
        this.timestamp = timestamp;
    }

    public static SeqTraceLog build(final Map<Object, Object> props) {
        final Object msgType = props.get(OtpUtil.atom("msg_type"));
        final Object serial = props.get(OtpUtil.atom("serial"));
        final Object from = props.get(OtpUtil.atom("from"));
        final Object to = props.get(OtpUtil.atom("to"));
        final Object message = props.get(OtpUtil.atom("message"));
        final Object timestamp = props.get(OtpUtil.atom("timestamp"));
        return new SeqTraceLog(msgType, serial, from, to, message, timestamp);
    }

    private static String stringValue(final Object obj) {
        if (obj instanceof OtpErlangString) return ((OtpErlangString) obj).stringValue();
        return obj.toString();
    }

    @Override
    public String toString() {
        return "SeqTraceLog [msg_type=" + this.msgType + ", serial=" + this.serial + ", from=" + this.from + ", to=" + this.to + ", message=" + this.message + "]";
    }

    public OtpErlangObject getMessage() {
        return (OtpErlangObject) this.message;
    }

    public Object getMsgType() {
        return this.msgType;
    }

    public Object getSerial() {
        return this.serial;
    }

    public Object getFrom() {
        return this.from;
    }

    public Object getTo() {
        return this.to;
    }

    public Object getTimestamp() {
        return this.timestamp;
    }
}
