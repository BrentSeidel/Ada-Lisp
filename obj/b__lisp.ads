pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: Community 2018 (20180523-73)" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_lisp" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#72fdc57c#;
   pragma Export (C, u00001, "lispB");
   u00002 : constant Version_32 := 16#050ff2f0#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#0f7d71d4#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#927a893f#;
   pragma Export (C, u00005, "ada__text_ioB");
   u00006 : constant Version_32 := 16#1ffab6e1#;
   pragma Export (C, u00006, "ada__text_ioS");
   u00007 : constant Version_32 := 16#f8088b52#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#16307b94#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#5726abed#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#41e5552e#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#085b6ffb#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#ae860117#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#4d58644d#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#bd45c2cc#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#4dcf97e2#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#86dbf443#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#40b73bd0#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#ced09590#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#259825ff#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#75bf515c#;
   pragma Export (C, u00020, "system__soft_links__initializeB");
   u00021 : constant Version_32 := 16#5697fc2b#;
   pragma Export (C, u00021, "system__soft_links__initializeS");
   u00022 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00022, "system__stack_checkingB");
   u00023 : constant Version_32 := 16#86e40413#;
   pragma Export (C, u00023, "system__stack_checkingS");
   u00024 : constant Version_32 := 16#34742901#;
   pragma Export (C, u00024, "system__exception_tableB");
   u00025 : constant Version_32 := 16#55f506b9#;
   pragma Export (C, u00025, "system__exception_tableS");
   u00026 : constant Version_32 := 16#ce4af020#;
   pragma Export (C, u00026, "system__exceptionsB");
   u00027 : constant Version_32 := 16#6038020d#;
   pragma Export (C, u00027, "system__exceptionsS");
   u00028 : constant Version_32 := 16#80916427#;
   pragma Export (C, u00028, "system__exceptions__machineB");
   u00029 : constant Version_32 := 16#3bad9081#;
   pragma Export (C, u00029, "system__exceptions__machineS");
   u00030 : constant Version_32 := 16#aa0563fc#;
   pragma Export (C, u00030, "system__exceptions_debugB");
   u00031 : constant Version_32 := 16#76d1963f#;
   pragma Export (C, u00031, "system__exceptions_debugS");
   u00032 : constant Version_32 := 16#6c2f8802#;
   pragma Export (C, u00032, "system__img_intB");
   u00033 : constant Version_32 := 16#0a808f39#;
   pragma Export (C, u00033, "system__img_intS");
   u00034 : constant Version_32 := 16#39df8c17#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#5679b13f#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#9ed49525#;
   pragma Export (C, u00036, "system__traceback_entriesB");
   u00037 : constant Version_32 := 16#0800998b#;
   pragma Export (C, u00037, "system__traceback_entriesS");
   u00038 : constant Version_32 := 16#bb296fbb#;
   pragma Export (C, u00038, "system__traceback__symbolicB");
   u00039 : constant Version_32 := 16#c84061d1#;
   pragma Export (C, u00039, "system__traceback__symbolicS");
   u00040 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00040, "ada__exceptions__tracebackB");
   u00041 : constant Version_32 := 16#20245e75#;
   pragma Export (C, u00041, "ada__exceptions__tracebackS");
   u00042 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00042, "system__address_imageB");
   u00043 : constant Version_32 := 16#a9b7f2c1#;
   pragma Export (C, u00043, "system__address_imageS");
   u00044 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00044, "system__wch_conB");
   u00045 : constant Version_32 := 16#13264d29#;
   pragma Export (C, u00045, "system__wch_conS");
   u00046 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00046, "system__wch_stwB");
   u00047 : constant Version_32 := 16#3e376128#;
   pragma Export (C, u00047, "system__wch_stwS");
   u00048 : constant Version_32 := 16#a831679c#;
   pragma Export (C, u00048, "system__wch_cnvB");
   u00049 : constant Version_32 := 16#1c91f7da#;
   pragma Export (C, u00049, "system__wch_cnvS");
   u00050 : constant Version_32 := 16#5ab55268#;
   pragma Export (C, u00050, "interfacesS");
   u00051 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00051, "system__wch_jisB");
   u00052 : constant Version_32 := 16#9ce1eefb#;
   pragma Export (C, u00052, "system__wch_jisS");
   u00053 : constant Version_32 := 16#10558b11#;
   pragma Export (C, u00053, "ada__streamsB");
   u00054 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00054, "ada__streamsS");
   u00055 : constant Version_32 := 16#92d882c5#;
   pragma Export (C, u00055, "ada__io_exceptionsS");
   u00056 : constant Version_32 := 16#d398a95f#;
   pragma Export (C, u00056, "ada__tagsB");
   u00057 : constant Version_32 := 16#12a0afb8#;
   pragma Export (C, u00057, "ada__tagsS");
   u00058 : constant Version_32 := 16#796f31f1#;
   pragma Export (C, u00058, "system__htableB");
   u00059 : constant Version_32 := 16#8c99dc11#;
   pragma Export (C, u00059, "system__htableS");
   u00060 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00060, "system__string_hashB");
   u00061 : constant Version_32 := 16#2ec7b76f#;
   pragma Export (C, u00061, "system__string_hashS");
   u00062 : constant Version_32 := 16#3cdd1378#;
   pragma Export (C, u00062, "system__unsigned_typesS");
   u00063 : constant Version_32 := 16#afdbf393#;
   pragma Export (C, u00063, "system__val_lluB");
   u00064 : constant Version_32 := 16#462f440a#;
   pragma Export (C, u00064, "system__val_lluS");
   u00065 : constant Version_32 := 16#269742a9#;
   pragma Export (C, u00065, "system__val_utilB");
   u00066 : constant Version_32 := 16#a4fbd905#;
   pragma Export (C, u00066, "system__val_utilS");
   u00067 : constant Version_32 := 16#ec4d5631#;
   pragma Export (C, u00067, "system__case_utilB");
   u00068 : constant Version_32 := 16#378ed9af#;
   pragma Export (C, u00068, "system__case_utilS");
   u00069 : constant Version_32 := 16#73d2d764#;
   pragma Export (C, u00069, "interfaces__c_streamsB");
   u00070 : constant Version_32 := 16#b1330297#;
   pragma Export (C, u00070, "interfaces__c_streamsS");
   u00071 : constant Version_32 := 16#4e0ce0a1#;
   pragma Export (C, u00071, "system__crtlS");
   u00072 : constant Version_32 := 16#ec083f01#;
   pragma Export (C, u00072, "system__file_ioB");
   u00073 : constant Version_32 := 16#af2a8e9e#;
   pragma Export (C, u00073, "system__file_ioS");
   u00074 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00074, "ada__finalizationS");
   u00075 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00075, "system__finalization_rootB");
   u00076 : constant Version_32 := 16#47a91c6b#;
   pragma Export (C, u00076, "system__finalization_rootS");
   u00077 : constant Version_32 := 16#0f8892f9#;
   pragma Export (C, u00077, "system__os_libB");
   u00078 : constant Version_32 := 16#d8e681fb#;
   pragma Export (C, u00078, "system__os_libS");
   u00079 : constant Version_32 := 16#2a8e89ad#;
   pragma Export (C, u00079, "system__stringsB");
   u00080 : constant Version_32 := 16#684d436e#;
   pragma Export (C, u00080, "system__stringsS");
   u00081 : constant Version_32 := 16#f5c4f553#;
   pragma Export (C, u00081, "system__file_control_blockS");
   u00082 : constant Version_32 := 16#edc89398#;
   pragma Export (C, u00082, "bbsS");
   u00083 : constant Version_32 := 16#a5c6d32e#;
   pragma Export (C, u00083, "bbs__lispB");
   u00084 : constant Version_32 := 16#6296cd1e#;
   pragma Export (C, u00084, "bbs__lispS");
   u00085 : constant Version_32 := 16#5b4659fa#;
   pragma Export (C, u00085, "ada__charactersS");
   u00086 : constant Version_32 := 16#8f637df8#;
   pragma Export (C, u00086, "ada__characters__handlingB");
   u00087 : constant Version_32 := 16#3b3f6154#;
   pragma Export (C, u00087, "ada__characters__handlingS");
   u00088 : constant Version_32 := 16#4b7bb96a#;
   pragma Export (C, u00088, "ada__characters__latin_1S");
   u00089 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00089, "ada__stringsS");
   u00090 : constant Version_32 := 16#96df1a3f#;
   pragma Export (C, u00090, "ada__strings__mapsB");
   u00091 : constant Version_32 := 16#1e526bec#;
   pragma Export (C, u00091, "ada__strings__mapsS");
   u00092 : constant Version_32 := 16#98e13b0e#;
   pragma Export (C, u00092, "system__bit_opsB");
   u00093 : constant Version_32 := 16#0765e3a3#;
   pragma Export (C, u00093, "system__bit_opsS");
   u00094 : constant Version_32 := 16#92f05f13#;
   pragma Export (C, u00094, "ada__strings__maps__constantsS");
   u00095 : constant Version_32 := 16#b40b1137#;
   pragma Export (C, u00095, "bbs__lisp__evaluateB");
   u00096 : constant Version_32 := 16#1a9229b5#;
   pragma Export (C, u00096, "bbs__lisp__evaluateS");
   u00097 : constant Version_32 := 16#60557a20#;
   pragma Export (C, u00097, "bbs__lisp__memoryB");
   u00098 : constant Version_32 := 16#a870727f#;
   pragma Export (C, u00098, "bbs__lisp__memoryS");
   u00099 : constant Version_32 := 16#fd83e873#;
   pragma Export (C, u00099, "system__concat_2B");
   u00100 : constant Version_32 := 16#0afbb82b#;
   pragma Export (C, u00100, "system__concat_2S");
   u00101 : constant Version_32 := 16#0b02bbfb#;
   pragma Export (C, u00101, "bbs__lisp__stringsB");
   u00102 : constant Version_32 := 16#b81aac88#;
   pragma Export (C, u00102, "bbs__lisp__stringsS");
   u00103 : constant Version_32 := 16#608e2cd1#;
   pragma Export (C, u00103, "system__concat_5B");
   u00104 : constant Version_32 := 16#8f052cd5#;
   pragma Export (C, u00104, "system__concat_5S");
   u00105 : constant Version_32 := 16#932a4690#;
   pragma Export (C, u00105, "system__concat_4B");
   u00106 : constant Version_32 := 16#763f44db#;
   pragma Export (C, u00106, "system__concat_4S");
   u00107 : constant Version_32 := 16#2b70b149#;
   pragma Export (C, u00107, "system__concat_3B");
   u00108 : constant Version_32 := 16#032b335e#;
   pragma Export (C, u00108, "system__concat_3S");
   u00109 : constant Version_32 := 16#273384e4#;
   pragma Export (C, u00109, "system__img_enum_newB");
   u00110 : constant Version_32 := 16#6917693b#;
   pragma Export (C, u00110, "system__img_enum_newS");
   u00111 : constant Version_32 := 16#af376c33#;
   pragma Export (C, u00111, "bbs__lisp__utilitiesB");
   u00112 : constant Version_32 := 16#4bd9c652#;
   pragma Export (C, u00112, "bbs__lisp__utilitiesS");
   u00113 : constant Version_32 := 16#ae7a7830#;
   pragma Export (C, u00113, "bbs__lisp__parserB");
   u00114 : constant Version_32 := 16#e51a3d72#;
   pragma Export (C, u00114, "bbs__lisp__parserS");
   u00115 : constant Version_32 := 16#d763507a#;
   pragma Export (C, u00115, "system__val_intB");
   u00116 : constant Version_32 := 16#40fe45c4#;
   pragma Export (C, u00116, "system__val_intS");
   u00117 : constant Version_32 := 16#1d9142a4#;
   pragma Export (C, u00117, "system__val_unsB");
   u00118 : constant Version_32 := 16#2c75fe43#;
   pragma Export (C, u00118, "system__val_unsS");
   u00119 : constant Version_32 := 16#fe1ffede#;
   pragma Export (C, u00119, "ada__strings__boundedB");
   u00120 : constant Version_32 := 16#89c18940#;
   pragma Export (C, u00120, "ada__strings__boundedS");
   u00121 : constant Version_32 := 16#7ec26662#;
   pragma Export (C, u00121, "ada__strings__superboundedB");
   u00122 : constant Version_32 := 16#da6addee#;
   pragma Export (C, u00122, "ada__strings__superboundedS");
   u00123 : constant Version_32 := 16#2eb48a6d#;
   pragma Export (C, u00123, "ada__strings__searchB");
   u00124 : constant Version_32 := 16#c1ab8667#;
   pragma Export (C, u00124, "ada__strings__searchS");
   u00125 : constant Version_32 := 16#acee74ad#;
   pragma Export (C, u00125, "system__compare_array_unsigned_8B");
   u00126 : constant Version_32 := 16#a1581e76#;
   pragma Export (C, u00126, "system__compare_array_unsigned_8S");
   u00127 : constant Version_32 := 16#a8025f3c#;
   pragma Export (C, u00127, "system__address_operationsB");
   u00128 : constant Version_32 := 16#1b57d1c8#;
   pragma Export (C, u00128, "system__address_operationsS");
   u00129 : constant Version_32 := 16#2323a8af#;
   pragma Export (C, u00129, "system__memoryB");
   u00130 : constant Version_32 := 16#512609cf#;
   pragma Export (C, u00130, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  ada.characters%s
   --  ada.characters.latin_1%s
   --  interfaces%s
   --  system%s
   --  system.address_operations%s
   --  system.address_operations%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.compare_array_unsigned_8%s
   --  system.compare_array_unsigned_8%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.concat_4%s
   --  system.concat_4%b
   --  system.concat_5%s
   --  system.concat_5%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.wch_stw%s
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  ada.exceptions%s
   --  system.wch_stw%b
   --  ada.exceptions.traceback%s
   --  system.secondary_stack%s
   --  system.address_image%s
   --  system.soft_links%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  system.exceptions%s
   --  system.exceptions%b
   --  ada.exceptions.last_chance_handler%s
   --  system.memory%s
   --  system.memory%b
   --  ada.exceptions.traceback%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  ada.exceptions.last_chance_handler%b
   --  system.standard_library%b
   --  ada.exceptions%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%s
   --  system.val_llu%b
   --  ada.tags%s
   --  ada.tags%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_uns%s
   --  system.val_uns%b
   --  system.val_int%s
   --  system.val_int%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  system.bit_ops%s
   --  system.bit_ops%b
   --  ada.strings.maps%s
   --  ada.strings.maps%b
   --  ada.strings.maps.constants%s
   --  ada.characters.handling%s
   --  ada.characters.handling%b
   --  ada.strings.search%s
   --  ada.strings.search%b
   --  ada.strings.superbounded%s
   --  ada.strings.superbounded%b
   --  ada.strings.bounded%s
   --  ada.strings.bounded%b
   --  bbs%s
   --  bbs.lisp%s
   --  bbs.lisp.utilities%s
   --  bbs.lisp.evaluate%s
   --  bbs.lisp.strings%s
   --  bbs.lisp.memory%s
   --  bbs.lisp.memory%b
   --  bbs.lisp.parser%s
   --  bbs.lisp.parser%b
   --  bbs.lisp%b
   --  bbs.lisp.utilities%b
   --  bbs.lisp.evaluate%b
   --  bbs.lisp.strings%b
   --  lisp%b
   --  END ELABORATION ORDER

end ada_main;
