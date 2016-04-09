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
package com.ericsson.otp.erlang;

import java.io.IOException;

public class OtpConn extends AbstractConnection {

    private final OtpSelfNode self2;

    protected OtpConn(OtpSelfNode self, OtpPeer other)
            throws IOException, OtpAuthException
    {
        super(self, other);

        self2 = self;

        // thread start
        start();
    }

    @Override
    public void deliver(final Exception e) {

    }

    @Override
    public void deliver(OtpMsg msg) {
        self2.deliver(msg);
    }

    public void send(OtpErlangPid pid, OtpErlangPid dest, OtpErlangObject msg)
        throws IOException
    {
        // encode and send the message
        super.sendBuf(pid, dest, new OtpOutputStream(msg));
    }

    public void send(OtpErlangPid pid, final String dest, final OtpErlangObject msg)
            throws IOException
    {
        // encode and send the message
        super.sendBuf(pid, dest, new OtpOutputStream(msg));
    }
}
