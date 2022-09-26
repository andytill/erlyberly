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
package com.ericsson.otp.erlang;

import java.io.IOException;

public class OtpSelfNode extends OtpNode {

    public OtpSelfNode(String node, String cookie) throws IOException {
        super(node, cookie);
    }

    public OtpSelfNode(String node, String cookie, int port) throws IOException {
        super(node, cookie, port);
    }

    public OtpSelfNode(String node) throws IOException {
        super(node);
    }

    public OtpConn connect(OtpPeer aPeer) throws OtpAuthException, IOException {
        return new OtpConn(this, aPeer);
    }
}
