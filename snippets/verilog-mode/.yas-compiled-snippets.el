;;; Compiled snippets and support files for `verilog-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'verilog-mode
                     '(("while" "while (${1:condition}) begin\n   $0\nend\n" "while" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/while" nil nil)
                       ("vtask" "/*! Description */\nvirtual task ${1:Name}();\n   $0\nendtask : $1\n" "virtual task" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/vtask" nil nil)
                       ("tbng" "/*! \\class $1_seq_c\n *  \\brief \n */\nclass ${1:SequenceName}_seq_c extends virtual_sequence_c;\n\n   /*! Register class with the factory */\n   `uvm_object_utils($1_seq_c);\n\n   const string report_id = \"${1:$(upcase text)}_SEQ\"; //!< Default id for messaging\n   \n   /*! Constructs a new $1_seq_c instance.\n    *\n    * \\param name sequence name\n    */\n   function new(string name = \"$1_seq_c\");\n      super.new(name);\n   endfunction : new\n\n   task body();\n      $0\n   endtask : body\n\nendclass : $1_seq_c\n\n" "TBNG Virtual Sequence" nil
                        ("tbng")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/virtseq" nil nil)
                       ("vfunc" "/*! Function Description\n *\n *  \\param <name> <description>\n *\n *  \\return <return value description>\n */\nvirtual function ${2:Type} ${1:Name}();\n   $0\nendfunction : $1\n\n" "virtual function" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/vfunc" nil nil)
                       ("union" "typedef union {\n   $0\n} ${1:name_u};\n" "union" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/union" nil nil)
                       ("testbench" "\\`include \"simParams.sv\"\n\nmodule tb_${1:module_name}();\n\nparameter STIMULI_FILE   = \\\"../../stimuli/$1_stimuli.txt\\\";\nparameter RESPONSE_FILE  = \\\"../../stimuli/$1_exp_responses.txt\\\";\n\ntypedef struct packed {\n/*AUTOLOGIC*/\n} response_t;\n\ntypedef struct packed{\n/*AUTOREGINPUT*/      \n} stimuli_t;\n\n\n//-------------------- Testbench signals --------------------\n\nlogic                                                             EndOfSim_S;\nstimuli_t             stimuli[];\nstimuli_t             stimulus;\nresponse_t            exp_responses[];\nresponse_t            exp_response;\n\nlogic                                                             clk;\nlogic                                                             rst;\n\ninteger                                                           error_counter;\ninteger                                                           total_counter;                  \n\nresponse_t actual_response;\n\n//--------------------- Instantiate MUT ---------------------\n\n\n/*   $1 AUTO_TEMPLATE (\n.\\\\\\(.*_.*\\\\\\)o         (actual_response.\\1o),\n.clk_i           (clk),\n.rst_ni          (rst),\n.\\\\\\(.*_.*\\\\\\)i         (stimulus.\\1i),\n);\n*/\n\n\n$1 #(\n/*AUTOINSTPARAM*/)\ni_mut\n(\n/*AUTOINST*/\n);\n\n\\`ifdef DUMMY_ON\n/*   $1 AUTO_TEMPLATE (\n\n.clk_i           (clk),\n.rst_ni          (rst),\n\n);\n*/\n\n$1 #(\n/*AUTOINSTPARAM*/)\ndummy                   \n(\n/*AUTOINST*/\n);\n`endif\n\n//------------------ Generate clock signal ------------------\n\ninitial begin\n\nclk = 0;\n#T_CLK;\nrst = 1'b1;\n#T_CLK;\nrst = 1'b0;\n#T_CLK;\nrst = 1'b1;\n#T_CLK;\n\ndo begin\nclk = 1'b1; #T_CLK_HI;\nclk = 1'b0; #T_CLK_LO;\nend while (EndOfSim_S == 1'b0);\nend\n\n//------------------- Stimuli Application -------------------\n\ninitial begin\nEndOfSim_S = 0;\nstimulus = '0;  \n//Read stimuli from file\n$display(\"Loading Stimuli from %s\", STIMULI_FILE);\n$readmemb(STIMULI_FILE, stimuli);\n\n//Apply the stimuli\nforeach(stimuli[i]) begin\n//Wait for one clock cycle\n@(posedge clk);\n//Delay application by the stimuli application delay\n#T_APPL_DEL;\nstimulus = stimuli[i];\nend\n\n//Wait one additional cycle for response acquisition to finish\n@(posedge clk);\n//Terminate simulation by stoping the clock\nEndOfSim_S = 1;\nend // initial begin\n\n//------------------- Response Acquisition -------------------\ninitial begin\n//Read expected responses\n$display(\"Loading expected responses from %s\", RESPONSE_FILE);\n$readmemb(RESPONSE_FILE, exp_responses);\nerror_counter = 0;\ntotal_counter = 0;\n\n//Compare responses in each cycle\nforeach(exp_responses[i]) begin\n//Wait for one clock cycle\n@(posedge clk);\n//Delay response acquistion by the stimuli acquistion delay\n#T_ACQ_DEL;\n\nexp_response = exp_responses[i]; //Use a helper signal to track the current expected response\n//in modelsim\n\n//Compare results\n// The ==? operator treats 'x' as don't care values wheras the normal == would\n// result with 'x'\ntotal_counter = total_counter + 1;\n\nif(actual_response  !=? exp_response) begin\n$error(\"Mismatch between expected and actual response. Was %b but should be %b, stimuli %d\", actual_response, exp_response,i);\nerror_counter = error_counter + 1;\nend  \nend\n\n$display(\"Tested %d stimuli\",total_counter); \n\nif(error_counter == 0) begin\n$display(\"No errors in testbench\");\nend else begin\n$display(\"%d errors in testbench\", error_counter);\nend\n\n$info(\"Simulation finished\");\nend\n\nendmodule : tb_$1\n" "testbench" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/testbench" nil nil)
                       ("tbng" "/*! \\class ${1:TestName}\n *  \\brief Provide a description of the test here\n *\n *  Provide a more detailed description here\n */\nclass $1 extends test_base_c;\n\n   const string report_id = \"${1:$(upcase text)}_SEQ\"; //!< Default id for messaging\n   \n   /*! Register class with the factory */\n   `uvm_component_utils($1);\n\n   /*! Create a new instance\n    *\n    * \\param name Name of the test\n    * \\param parent Parent component\n    */\n   function new(string name = \"$BASE$\", uvm_component parent = null);\n      super.new(name, parent);\n   endfunction : new\n\n   /*! Sets the config values\n    */\n   function void set_config_values();\n      super.set_config_values();\n\n      // Set config variables here\n      // m_ebi_cfg_h.enable_monitor_recording = 0;\n      // m_ebi_cfg_h.has_scoreboard = 0;\n      // m_uart_cfg_h.enable_monitor_recording = 0;\n\n      // Set simulation timeout to the \n      set_global_timeout(200us);\n      \n   endfunction : set_config_values\n   \n   /*! Run the test\n    */\n   task main_phase(uvm_phase phase);\n\n      phase.raise_objection(this);\n\n      // Write test here\n      $0\n\n      phase.drop_objection(this); \n   endtask : run\n   \nendclass : $1\n\n/*!@}*/\n" "TBNG Test" nil
                        ("tbng")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/test" nil nil)
                       ("task" "/*! Description */\ntask ${1:Name}();\n   $0\nendtask : $1\n" "task" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/task" nil nil)
                       ("submodule" "${2:submodule_name} #(\n                               /*AUTOINSTPARAM*/\n                                )\n ${1:name}\n(\n                               /*AUTOINST*/\n                                 );\n" "submodule" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/submodule" nil nil)
                       ("struct" "/*! Typedef Description */\ntypedef struct {\n    $0;    //!< Field Description\n} ${1:Name};\n\n" "struct" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/struct" nil nil)
                       ("repeat" "repeat (${1:Condition}) begin\n   $0\nend\n" "repeat" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/repeat" nil nil)
                       ("package" "package ${1:package_name};\n   $0\n\nendpackage: $1\n\n" "package" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/package" nil nil)
                       ("module" "module ${1:module_name} #(\n)(\n\n);\n   $0\n   /*AUTOLOGIC*/\n   \nendmodule: $1\n\n" "module" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/module" nil nil)
                       ("interface" "interface ${1:Name} ();\n   $0\nendinterface : $1\n\n" "interface" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/interface" nil nil)
                       ("if" "if ($1) begin\n\nend else if ($0) begin\n   \nend else begin\n\nend\n" "if" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/if" nil nil)
                       ("func" "/*! Function Description\n *\n *  \\param <name> <description>\n *\n *  \\return <return value description>\n */\nfunction ${2:Type} ${1:Name}();\n   $0\nendfunction : $1\n" "function" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/func" nil nil)
                       ("fork" "fork\n   begin : ${1:label_1}\n      $0\n   end\n   begin : ${2:label_2}\n   end\njoin_none\n#0;\n" "fork/join_none" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/fork_none" nil nil)
                       ("fork" "fork\n   begin : ${1:label_1}\n      $0\n   end\n   begin : ${2:label_2}\n   end\njoin\n" "fork/join" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/fork_join" nil nil)
                       ("fork" "fork\n   begin : ${1:label_1}\n      $0\n   end\n   begin : ${2:label_2}\n   end\njoin_any\n" "fork/join_any" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/fork_any" nil nil)
                       ("for" "for ($0; ; ) begin\nend\n" "for" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/for" nil nil)
                       ("enum" "/*! Description */\ntypedef enum ${2:Type} {\n   ${3:Value} = $0;  //!< Value Description\n} ${1:Name};\n\n" "enum" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/enum" nil nil)
                       ("do" "do begin\n   $0\nend while($1);\n\n" "do" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/do" nil nil)
                       ("dependency_footer" "// Local Variables:\n// verilog-typedef-regexp: \"_t$\"\n// verilog-library-flags:(\"-y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl/tec_dep -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl/HiMax -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl/spi_slave -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl/DVS -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl/params -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/tb -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/generic_xbar/src -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/common_cells/formal -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/common_cells/src -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/common_cells/src/deprecated -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/common_cells/src -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/common_cells/test -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/tb -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/pkg -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/spi_slave -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/test -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/tec_dep -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/unpacker -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/cpi -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/xbar -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/regfile -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/test_unit -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/spi_slave -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/msb/fe/rtl/components/clkgen -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/axi_slice_dc/src -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/source/sourcecode -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/source/sourcecode/tb -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/source/sourcecode -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/spi_fifo/misc -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/spi_fifo/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/spi_fifo/testbench -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/frame_memory/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/frame_memory/testbench -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/dvsi/frame_memory/misc -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/tb -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/rtl/spi_slave -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/rtl/cfg_regs -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/rtl/VCB_1IGLOO/hdl_HPAddrorGreyStringReadout/hdlfiles -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/rtl/VCB_1IGLOO/hdl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/rtl/dc_fifos -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/vcbi/rtl -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/udma/rtl/common -y /home/scheremo/PhD/ArmaSuisse/sensorbridge/IP/udma/rtl/core \")\n// End:\n" "dependency_footer" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/dependency_footer" nil nil)
                       ("covergroup" "covergroup ${1:CoverGroup};\n   coverpoint name {\n      $0\n   };\nendgroup\n" "covergroup" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/covergroup" nil nil)
                       ("copyright-header" "" "copyright-header" nil nil nil "/home/scheremo/.emacs.d/snippets/verilog-mode/copyright-header" nil nil)
                       ("clocking" "clocking ${1:name_cb} @(posedge ${2:clock});\n   default input #${3:setup_time} output #${4:hold_time};\n   $0\n   // output declarations\n   // input declarations\nendclocking: $1\n\n" "clocking" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/clocking" nil nil)
                       ("class" "class ${1:Name} extends $2;\n\n   function new;\n      $0\n   endfunction : new\n\nendclass : $1\n" "class" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/class" nil nil)
                       ("case" "case (${1:Expression}) \n   ${2:Condition}: \n      begin\n         $0\n      end\n   default:\n      begin\n      end \nendcase" "case" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/case" nil nil)
                       ("author_header" "// ----------------------------------------------------------------------\n //\n // File: `(file-name-nondirectory (buffer-file-name))`$0\n //\n // Last edited: `(format-time-string \"%d.%m.%Y\")`$0        \n // \n // Copyright (C) `(format-time-string \"%Y\")`$0, ETH Zurich and University of Bologna.\n //\n // Author: Moritz Scherer, ETH Zurich\n //\n // \n // SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1\n //\n // Licensed under the Solderpad Hardware License v 2.1 (the \"License\"); you may not use this file except in compliance with the License, or, at your option, the Apache License version 2.0.\n //\n // You may obtain a copy of the License at\n // https://solderpad.org/licenses/SHL-2.1/\n // Unless required by applicable law or agreed to in writing, any work distributed under the License is distributed on an \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n //\n // See the License for the specific language governing permissions and limitations under the License.\n //\n // ----------------------------------------------------------------------\n" "author_header" nil nil nil "/home/scheremo/.emacs.d/snippets/verilog-mode/author_header" nil nil)
                       ("assert" "property ${1:PropertyName};\n   @(posedge ${2:CLK}) $0/* condition |=> value*/ ;\nendproperty\n\nassert_$1: assert property ($1);\n" "assert" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/assert" nil nil)
                       ("tbng" "/*! \\class $1_$2_seq_c\n *  \\brief \n *  \\ingroup $1_sequence\n */\nclass $1_${2:Seq}_seq_c extends ${1:Agent}_seq_base_c;\n\n  /*! Register class with the factor */\n  `uvm_object_utils($1_$2_seq_c)\n\n  /*! Constructs a new $1_$2_seq_c instance.\n   *\n   * \\param name sequence name\n   */\n  function new(string name = \"$1_$2_write_seq_c\");\n     super.new(name);\n  endfunction : new\n\n  /*! Body description \n   */\n  task body();\n     req = $1_item_c::type_id::create(\"req\");\n\n     start_item(req);\n     $0\n     finish_item(req);\n     get_response(rsp);\n   endtask : body\n\nendclass : $1_$2_seq_c\n" "TBNG API Seqence" nil
                        ("tbng")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/apiseq" nil nil)
                       ("always_ff" "always_ff @(posedge ${1:clk_i} or negedge ${2:rst_ni}) begin\n   if (~$2) begin\n      $0\n   end else begin\n  \n   end\nend" "always_ff" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/always_ff" nil nil)
                       ("always_comb" "always_comb begin\n\nend" "always_comb" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/always_comb" nil nil)
                       ("always" "always @(posedge ${1:clock_i} or negedge ${2:reset_ni}) begin\n   if (~$2) begin\n      $0\n   end else begin\n  \n   end\nend" "always @()" nil
                        ("verilog")
                        nil "/home/scheremo/.emacs.d/snippets/verilog-mode/always" nil nil)))


;;; Do not edit! File generated at Wed Nov 11 10:01:37 2020
