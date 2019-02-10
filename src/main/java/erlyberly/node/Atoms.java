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
package erlyberly.node;

import static erlyberly.node.OtpUtil.atom;

import com.ericsson.otp.erlang.OtpErlangAtom;

/**
 * Atom constants.
 */
public class Atoms {
    public static final OtpErlangAtom ERLANG_ATOM = atom("erlang");
    public static final OtpErlangAtom REGISTER_ATOM = atom("register");
    public static final OtpErlangAtom CODE_SERVER_ATOM = atom("code_server");
    public static final OtpErlangAtom TRY_LOAD_MODULE_ATOM = atom("try_load_module");
    public static final OtpErlangAtom RETURN_FROM_ATOM = atom("return_from");
    public static final OtpErlangAtom EXCEPTION_FROM_ATOM = atom("exception_from");
    public static final OtpErlangAtom CALL_ATOM = atom("call");
    public static final OtpErlangAtom UNREGISTER_ATOM = atom("unregister");
}
