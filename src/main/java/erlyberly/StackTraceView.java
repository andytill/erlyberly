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
import erlyberly.StackTraceView.ErlyberlyStackTraceElement;
import erlyberly.node.OtpUtil;
import javafx.application.Platform;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.util.Callback;

public class StackTraceView extends ListView<ErlyberlyStackTraceElement> {
    @SuppressWarnings("rawtypes")
    public StackTraceView() {
        super();
        this.applyStackTraceCellFactory(this);
    }

    private void applyStackTraceCellFactory(final ListView<ErlyberlyStackTraceElement> listview) {
        listview.setCellFactory(new Callback<ListView<ErlyberlyStackTraceElement>, ListCell<ErlyberlyStackTraceElement>>() {
            @Override
            public ListCell<ErlyberlyStackTraceElement> call(final ListView<ErlyberlyStackTraceElement> param) {
                return new ListCell<ErlyberlyStackTraceElement>() {
                    private final Hyperlink functionLink = new Hyperlink();

                    {
                        this.setGraphic(this.functionLink);
                        this.functionLink.setOnAction((e) -> {
                            final ErlyberlyStackTraceElement stackElement = this.getItem();
                            if (null == stackElement) return;
                            if (stackElement.isError()) return;
                            final ModFunc mf = stackElement.getModFunc();
                            ErlyBerly.runIO(() -> {
                                try {
                                    final String source = ErlyBerly.nodeAPI().moduleFunctionSourceCode(mf.getModuleName(), mf.getFuncName(), mf.getArity());
                                    Platform.runLater(() -> ErlyBerly.showPane("Crash Report Stack", ErlyBerly.wrapInPane(new CodeView(source))));
                                } catch (final Exception e1) {
                                    e1.printStackTrace();
                                }
                            });
                        });
                    }

                    @Override
                    public void updateItem(final ErlyberlyStackTraceElement item, final boolean empty) {
                        super.updateItem(item, empty);
                        if (null == item) this.functionLink.setText("");
                        else this.functionLink.setText(item.toString());
                    }
                };
            }
        });
    }

    public void populateFromCrashReport(final CrashReport crashReport) {
        try {
            this.getItems().addAll(crashReport.mapStackTraces((module, function, arity, file, line) -> {
                try {
                    final ModFunc modFunc = StackTraceView.mfaToModFunc(module, function, arity);
                    return new ErlyberlyStackTraceElement(modFunc, file, line.longValue());
                } catch (final Exception e) {
                    throw new RuntimeException(e);
                }
            }));
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    private static ModFunc mfaToModFunc(final OtpErlangAtom module, final OtpErlangAtom function, final OtpErlangLong arity) {
        final boolean exported = false;
        final boolean synthetic = false;
        try {
            return new ModFunc(module.toString(), function.toString(), arity.intValue(), exported, synthetic);
        } catch (final OtpErlangRangeException e) {
            throw new RuntimeException(e);
        }
    }

    public void populateFromMfaList(final OtpErlangList stackTrace) {
        try {
            for (final OtpErlangObject obj : stackTrace) {
                if (obj instanceof OtpErlangString) {
                    final OtpErlangString errorMessage = (OtpErlangString) obj;
                    this.getItems().add(new ErlyberlyStackTraceElement(errorMessage.stringValue()));
                } else {
                    final OtpErlangTuple tuple = OtpUtil.toTuple(obj);
                    final OtpErlangAtom module = (OtpErlangAtom) tuple.elementAt(0);
                    final OtpErlangAtom function = (OtpErlangAtom) tuple.elementAt(1);
                    final OtpErlangLong arity = (OtpErlangLong) tuple.elementAt(2);
                    final ModFunc modFunc = StackTraceView.mfaToModFunc(module, function, arity);
                    this.getItems().add(new ErlyberlyStackTraceElement(modFunc, "", 0L));
                }
            }
        } catch (final Exception e) {
            // try/catch so if there is a problem decoding the process_dump, it won't
            // stop the trace log from being shown.
            e.printStackTrace();
        }
    }

    public boolean isStackTracesEmpty() {
        return this.getItems().isEmpty();
    }

    /**
     * Put erlyberly on the front of this because there is already a StaceTraceElement
     * class in the standard library.
     */
    static class ErlyberlyStackTraceElement {
        private final ModFunc modFunc;
        private final long line;
        private final String file;
        private final String error;

        ErlyberlyStackTraceElement(final ModFunc modFunc, final String file, final long line) {
            super();
            this.modFunc = modFunc;
            this.file = file;
            this.line = line;
            this.error = null;
        }

        ErlyberlyStackTraceElement(final String anError) {
            super();
            this.modFunc = null;
            this.file = null;
            this.line = -1L;
            this.error = anError;
        }

        public ModFunc getModFunc() {
            return this.modFunc;
        }

        public boolean isError() {
            return null != this.error;
        }

        @Override
        public String toString() {
            if (this.isError()) {
                return this.error;
            }
            assert null != this.modFunc;
            String display = this.modFunc.toFullString();
            if (null != this.file && !this.file.isEmpty()) {
                display += "  (" + this.file + ":" + this.line + ")";
            }
            return display;
        }
    }
}
