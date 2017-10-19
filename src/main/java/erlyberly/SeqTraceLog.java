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

import java.util.Map;

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

    public SeqTraceLog(Object msgType, Object serial, Object from, Object to, Object message, Object timestamp) {
        this.msgType = msgType;
        this.serial = serial;
        this.from = stringValue(from);
        this.to = stringValue(to);
        this.message = message;
        this.timestamp = timestamp;
    }

    public static SeqTraceLog build(Map<OtpErlangObject, OtpErlangObject> props) {
        Object msgType = props.get(OtpUtil.atom("msg_type"));
        Object serial = props.get(OtpUtil.atom("serial"));
        Object from = props.get(OtpUtil.atom("from"));
        Object to = props.get(OtpUtil.atom("to"));
        Object message = props.get(OtpUtil.atom("message"));
        Object timestamp = props.get(OtpUtil.atom("timestamp"));
        return new SeqTraceLog(msgType, serial,  from, to, message, timestamp);
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
