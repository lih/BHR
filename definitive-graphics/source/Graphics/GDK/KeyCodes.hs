{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fmax-pmcheck-iterations=10000000 #-}
#endif
#endif
module Graphics.GDK.KeyCodes where

import Definitive

data KeyCode = GDK_KEY_VoidSymbol | GDK_KEY_BackSpace | GDK_KEY_Tab | GDK_KEY_Linefeed
             | GDK_KEY_Clear | GDK_KEY_Return | GDK_KEY_Pause | GDK_KEY_Scroll_Lock
             | GDK_KEY_Sys_Req | GDK_KEY_Escape | GDK_KEY_Delete | GDK_KEY_Multi_key
             | GDK_KEY_Codeinput | GDK_KEY_SingleCandidate | GDK_KEY_MultipleCandidate
             | GDK_KEY_PreviousCandidate | GDK_KEY_Kanji | GDK_KEY_Muhenkan | GDK_KEY_Henkan_Mode
             | GDK_KEY_Henkan | GDK_KEY_Romaji | GDK_KEY_Hiragana | GDK_KEY_Katakana
             | GDK_KEY_Hiragana_Katakana | GDK_KEY_Zenkaku | GDK_KEY_Hankaku | GDK_KEY_Zenkaku_Hankaku
             | GDK_KEY_Touroku | GDK_KEY_Massyo | GDK_KEY_Kana_Lock | GDK_KEY_Kana_Shift | GDK_KEY_Eisu_Shift
             | GDK_KEY_Eisu_toggle | GDK_KEY_Kanji_Bangou | GDK_KEY_Zen_Koho | GDK_KEY_Mae_Koho
             | GDK_KEY_Home | GDK_KEY_Left | GDK_KEY_Up | GDK_KEY_Right | GDK_KEY_Down
             | GDK_KEY_Prior | GDK_KEY_Page_Up | GDK_KEY_Next | GDK_KEY_Page_Down
             | GDK_KEY_End | GDK_KEY_Begin | GDK_KEY_Select | GDK_KEY_Print | GDK_KEY_Execute
             | GDK_KEY_Insert | GDK_KEY_Undo | GDK_KEY_Redo | GDK_KEY_Menu | GDK_KEY_Find
             | GDK_KEY_Cancel | GDK_KEY_Help | GDK_KEY_Break | GDK_KEY_Mode_switch | GDK_KEY_script_switch
             | GDK_KEY_Num_Lock | GDK_KEY_KP_Space | GDK_KEY_KP_Tab | GDK_KEY_KP_Enter
             | GDK_KEY_KP_F1 | GDK_KEY_KP_F2 | GDK_KEY_KP_F3 | GDK_KEY_KP_F4
             | GDK_KEY_KP_Home | GDK_KEY_KP_Left | GDK_KEY_KP_Up | GDK_KEY_KP_Right
             | GDK_KEY_KP_Down | GDK_KEY_KP_Prior | GDK_KEY_KP_Page_Up | GDK_KEY_KP_Next
             | GDK_KEY_KP_Page_Down | GDK_KEY_KP_End | GDK_KEY_KP_Begin | GDK_KEY_KP_Insert
             | GDK_KEY_KP_Delete | GDK_KEY_KP_Equal | GDK_KEY_KP_Multiply | GDK_KEY_KP_Add
             | GDK_KEY_KP_Separator | GDK_KEY_KP_Subtract | GDK_KEY_KP_Decimal | GDK_KEY_KP_Divide
             | GDK_KEY_KP_0 | GDK_KEY_KP_1 | GDK_KEY_KP_2 | GDK_KEY_KP_3 | GDK_KEY_KP_4 | GDK_KEY_KP_5
             | GDK_KEY_KP_6 | GDK_KEY_KP_7 | GDK_KEY_KP_8 | GDK_KEY_KP_9 | GDK_KEY_F1 | GDK_KEY_F2
             | GDK_KEY_F3 | GDK_KEY_F4 | GDK_KEY_F5 | GDK_KEY_F6 | GDK_KEY_F7 | GDK_KEY_F8
             | GDK_KEY_F9 | GDK_KEY_F10 | GDK_KEY_F11 | GDK_KEY_L1 | GDK_KEY_F12 | GDK_KEY_L2
             | GDK_KEY_F13 | GDK_KEY_L3 | GDK_KEY_F14 | GDK_KEY_L4 | GDK_KEY_F15 | GDK_KEY_L5
             | GDK_KEY_F16 | GDK_KEY_L6 | GDK_KEY_F17 | GDK_KEY_L7 | GDK_KEY_F18 | GDK_KEY_L8
             | GDK_KEY_F19 | GDK_KEY_L9 | GDK_KEY_F20 | GDK_KEY_L10 | GDK_KEY_F21 | GDK_KEY_R1
             | GDK_KEY_F22 | GDK_KEY_R2 | GDK_KEY_F23 | GDK_KEY_R3 | GDK_KEY_F24 | GDK_KEY_R4
             | GDK_KEY_F25 | GDK_KEY_R5 | GDK_KEY_F26 | GDK_KEY_R6 | GDK_KEY_F27 | GDK_KEY_R7
             | GDK_KEY_F28 | GDK_KEY_R8 | GDK_KEY_F29 | GDK_KEY_R9 | GDK_KEY_F30 | GDK_KEY_R10
             | GDK_KEY_F31 | GDK_KEY_R11 | GDK_KEY_F32 | GDK_KEY_R12 | GDK_KEY_F33 | GDK_KEY_R13
             | GDK_KEY_F34 | GDK_KEY_R14 | GDK_KEY_F35 | GDK_KEY_R15 | GDK_KEY_Shift_L | GDK_KEY_Shift_R
             | GDK_KEY_Control_L | GDK_KEY_Control_R | GDK_KEY_Caps_Lock | GDK_KEY_Shift_Lock | GDK_KEY_Meta_L | GDK_KEY_Meta_R
             | GDK_KEY_Alt_L | GDK_KEY_Alt_R | GDK_KEY_Super_L | GDK_KEY_Super_R | GDK_KEY_Hyper_L
             | GDK_KEY_Hyper_R | GDK_KEY_ISO_Lock | GDK_KEY_ISO_Level2_Latch | GDK_KEY_ISO_Level3_Shift
             | GDK_KEY_ISO_Level3_Latch | GDK_KEY_ISO_Level3_Lock | GDK_KEY_ISO_Level5_Shift
             | GDK_KEY_ISO_Level5_Latch | GDK_KEY_ISO_Level5_Lock | GDK_KEY_ISO_Group_Shift
             | GDK_KEY_ISO_Group_Latch | GDK_KEY_ISO_Group_Lock | GDK_KEY_ISO_Next_Group
             | GDK_KEY_ISO_Next_Group_Lock | GDK_KEY_ISO_Prev_Group | GDK_KEY_ISO_Prev_Group_Lock
             | GDK_KEY_ISO_First_Group | GDK_KEY_ISO_First_Group_Lock | GDK_KEY_ISO_Last_Group
             | GDK_KEY_ISO_Last_Group_Lock | GDK_KEY_ISO_Left_Tab | GDK_KEY_ISO_Move_Line_Up
             | GDK_KEY_ISO_Move_Line_Down | GDK_KEY_ISO_Partial_Line_Up | GDK_KEY_ISO_Partial_Line_Down
             | GDK_KEY_ISO_Partial_Space_Left | GDK_KEY_ISO_Partial_Space_Right | GDK_KEY_ISO_Set_Margin_Left
             | GDK_KEY_ISO_Set_Margin_Right | GDK_KEY_ISO_Release_Margin_Left | GDK_KEY_ISO_Release_Margin_Right
             | GDK_KEY_ISO_Release_Both_Margins | GDK_KEY_ISO_Fast_Cursor_Left
             | GDK_KEY_ISO_Fast_Cursor_Right | GDK_KEY_ISO_Fast_Cursor_Up | GDK_KEY_ISO_Fast_Cursor_Down
             | GDK_KEY_ISO_Continuous_Underline | GDK_KEY_ISO_Discontinuous_Underline | GDK_KEY_ISO_Emphasize
             | GDK_KEY_ISO_Center_Object | GDK_KEY_ISO_Enter | GDK_KEY_dead_grave
             | GDK_KEY_dead_acute | GDK_KEY_dead_circumflex | GDK_KEY_dead_tilde
             | GDK_KEY_dead_perispomeni | GDK_KEY_dead_macron | GDK_KEY_dead_breve | GDK_KEY_dead_abovedot
             | GDK_KEY_dead_diaeresis | GDK_KEY_dead_abovering | GDK_KEY_dead_doubleacute | GDK_KEY_dead_caron
             | GDK_KEY_dead_cedilla | GDK_KEY_dead_ogonek | GDK_KEY_dead_iota | GDK_KEY_dead_voiced_sound
             | GDK_KEY_dead_semivoiced_sound | GDK_KEY_dead_belowdot | GDK_KEY_dead_hook | GDK_KEY_dead_horn
             | GDK_KEY_dead_stroke | GDK_KEY_dead_abovecomma | GDK_KEY_dead_psili | GDK_KEY_dead_abovereversedcomma
             | GDK_KEY_dead_dasia | GDK_KEY_dead_doublegrave | GDK_KEY_dead_belowring
             | GDK_KEY_dead_belowmacron | GDK_KEY_dead_belowcircumflex | GDK_KEY_dead_belowtilde
             | GDK_KEY_dead_belowbreve | GDK_KEY_dead_belowdiaeresis | GDK_KEY_dead_invertedbreve
             | GDK_KEY_dead_belowcomma | GDK_KEY_dead_currency | GDK_KEY_dead_a
             | GDK_KEY_dead_A | GDK_KEY_dead_e | GDK_KEY_dead_E | GDK_KEY_dead_i
             | GDK_KEY_dead_I | GDK_KEY_dead_o | GDK_KEY_dead_O | GDK_KEY_dead_u
             | GDK_KEY_dead_U | GDK_KEY_dead_small_schwa | GDK_KEY_dead_capital_schwa | GDK_KEY_dead_greek
             | GDK_KEY_First_Virtual_Screen | GDK_KEY_Prev_Virtual_Screen | GDK_KEY_Next_Virtual_Screen | GDK_KEY_Last_Virtual_Screen
             | GDK_KEY_Terminate_Server | GDK_KEY_AccessX_Enable | GDK_KEY_AccessX_Feedback_Enable
             | GDK_KEY_RepeatKeys_Enable | GDK_KEY_SlowKeys_Enable | GDK_KEY_BounceKeys_Enable
             | GDK_KEY_StickyKeys_Enable | GDK_KEY_MouseKeys_Enable | GDK_KEY_MouseKeys_Accel_Enable
             | GDK_KEY_Overlay1_Enable | GDK_KEY_Overlay2_Enable | GDK_KEY_AudibleBell_Enable
             | GDK_KEY_Pointer_Left | GDK_KEY_Pointer_Right | GDK_KEY_Pointer_Up
             | GDK_KEY_Pointer_Down | GDK_KEY_Pointer_UpLeft | GDK_KEY_Pointer_UpRight
             | GDK_KEY_Pointer_DownLeft | GDK_KEY_Pointer_DownRight | GDK_KEY_Pointer_Button_Dflt
             | GDK_KEY_Pointer_Button1 | GDK_KEY_Pointer_Button2 | GDK_KEY_Pointer_Button3
             | GDK_KEY_Pointer_Button4 | GDK_KEY_Pointer_Button5 | GDK_KEY_Pointer_DblClick_Dflt
             | GDK_KEY_Pointer_DblClick1 | GDK_KEY_Pointer_DblClick2 | GDK_KEY_Pointer_DblClick3
             | GDK_KEY_Pointer_DblClick4 | GDK_KEY_Pointer_DblClick5 | GDK_KEY_Pointer_Drag_Dflt
             | GDK_KEY_Pointer_Drag1 | GDK_KEY_Pointer_Drag2 | GDK_KEY_Pointer_Drag3
             | GDK_KEY_Pointer_Drag4 | GDK_KEY_Pointer_Drag5 | GDK_KEY_Pointer_EnableKeys
             | GDK_KEY_Pointer_Accelerate | GDK_KEY_Pointer_DfltBtnNext | GDK_KEY_Pointer_DfltBtnPrev
             | GDK_KEY_ch | GDK_KEY_Ch | GDK_KEY_CH | GDK_KEY_c_h | GDK_KEY_C_h | GDK_KEY_C_H
             | GDK_KEY_3270_Duplicate | GDK_KEY_3270_FieldMark | GDK_KEY_3270_Right2 | GDK_KEY_3270_Left2
             | GDK_KEY_3270_BackTab | GDK_KEY_3270_EraseEOF | GDK_KEY_3270_EraseInput | GDK_KEY_3270_Reset
             | GDK_KEY_3270_Quit | GDK_KEY_3270_PA1 | GDK_KEY_3270_PA2 | GDK_KEY_3270_PA3
             | GDK_KEY_3270_Test | GDK_KEY_3270_Attn | GDK_KEY_3270_CursorBlink | GDK_KEY_3270_AltCursor | GDK_KEY_3270_KeyClick
             | GDK_KEY_3270_Jump | GDK_KEY_3270_Ident | GDK_KEY_3270_Rule | GDK_KEY_3270_Copy | GDK_KEY_3270_Play
             | GDK_KEY_3270_Setup | GDK_KEY_3270_Record | GDK_KEY_3270_ChangeScreen | GDK_KEY_3270_DeleteWord | GDK_KEY_3270_ExSelect
             | GDK_KEY_3270_CursorSelect | GDK_KEY_3270_PrintScreen | GDK_KEY_3270_Enter | GDK_KEY_space
             | GDK_KEY_exclam | GDK_KEY_quotedbl | GDK_KEY_numbersign | GDK_KEY_dollar | GDK_KEY_percent
             | GDK_KEY_ampersand | GDK_KEY_apostrophe | GDK_KEY_quoteright | GDK_KEY_parenleft | GDK_KEY_parenright
             | GDK_KEY_asterisk | GDK_KEY_plus | GDK_KEY_comma | GDK_KEY_minus | GDK_KEY_period
             | GDK_KEY_slash | GDK_KEY_0 | GDK_KEY_1 | GDK_KEY_2 | GDK_KEY_3 | GDK_KEY_4 | GDK_KEY_5
             | GDK_KEY_6 | GDK_KEY_7 | GDK_KEY_8 | GDK_KEY_9 | GDK_KEY_colon | GDK_KEY_semicolon | GDK_KEY_less
             | GDK_KEY_equal | GDK_KEY_greater | GDK_KEY_question | GDK_KEY_at | GDK_KEY_A | GDK_KEY_B | GDK_KEY_C
             | GDK_KEY_D | GDK_KEY_E | GDK_KEY_F | GDK_KEY_G | GDK_KEY_H | GDK_KEY_I | GDK_KEY_J | GDK_KEY_K | GDK_KEY_L
             | GDK_KEY_M | GDK_KEY_N | GDK_KEY_O | GDK_KEY_P | GDK_KEY_Q | GDK_KEY_R | GDK_KEY_S | GDK_KEY_T
             | GDK_KEY_U | GDK_KEY_V | GDK_KEY_W | GDK_KEY_X | GDK_KEY_Y | GDK_KEY_Z | GDK_KEY_bracketleft
             | GDK_KEY_backslash | GDK_KEY_bracketright | GDK_KEY_asciicircum | GDK_KEY_underscore | GDK_KEY_grave
             | GDK_KEY_quoteleft | GDK_KEY_a | GDK_KEY_b | GDK_KEY_c | GDK_KEY_d | GDK_KEY_e | GDK_KEY_f
             | GDK_KEY_g | GDK_KEY_h | GDK_KEY_i | GDK_KEY_j | GDK_KEY_k | GDK_KEY_l | GDK_KEY_m | GDK_KEY_n
             | GDK_KEY_o | GDK_KEY_p | GDK_KEY_q | GDK_KEY_r | GDK_KEY_s | GDK_KEY_t | GDK_KEY_u | GDK_KEY_v
             | GDK_KEY_w | GDK_KEY_x | GDK_KEY_y | GDK_KEY_z | GDK_KEY_braceleft | GDK_KEY_bar
             | GDK_KEY_braceright | GDK_KEY_asciitilde | GDK_KEY_nobreakspace | GDK_KEY_exclamdown | GDK_KEY_cent
             | GDK_KEY_sterling | GDK_KEY_currency | GDK_KEY_yen | GDK_KEY_brokenbar | GDK_KEY_section | GDK_KEY_diaeresis
             | GDK_KEY_copyright | GDK_KEY_ordfeminine | GDK_KEY_guillemotleft | GDK_KEY_notsign | GDK_KEY_hyphen
             | GDK_KEY_registered | GDK_KEY_macron | GDK_KEY_degree | GDK_KEY_plusminus | GDK_KEY_twosuperior
             | GDK_KEY_threesuperior | GDK_KEY_acute | GDK_KEY_mu | GDK_KEY_paragraph | GDK_KEY_periodcentered
             | GDK_KEY_cedilla | GDK_KEY_onesuperior | GDK_KEY_masculine | GDK_KEY_guillemotright | GDK_KEY_onequarter
             | GDK_KEY_onehalf | GDK_KEY_threequarters | GDK_KEY_questiondown | GDK_KEY_Agrave | GDK_KEY_Aacute
             | GDK_KEY_Acircumflex | GDK_KEY_Atilde | GDK_KEY_Adiaeresis | GDK_KEY_Aring | GDK_KEY_AE
             | GDK_KEY_Ccedilla | GDK_KEY_Egrave | GDK_KEY_Eacute | GDK_KEY_Ecircumflex | GDK_KEY_Ediaeresis
             | GDK_KEY_Igrave | GDK_KEY_Iacute | GDK_KEY_Icircumflex | GDK_KEY_Idiaeresis | GDK_KEY_ETH
             | GDK_KEY_Eth | GDK_KEY_Ntilde | GDK_KEY_Ograve | GDK_KEY_Oacute | GDK_KEY_Ocircumflex
             | GDK_KEY_Otilde | GDK_KEY_Odiaeresis | GDK_KEY_multiply | GDK_KEY_Oslash | GDK_KEY_Ooblique
             | GDK_KEY_Ugrave | GDK_KEY_Uacute | GDK_KEY_Ucircumflex | GDK_KEY_Udiaeresis | GDK_KEY_Yacute
             | GDK_KEY_THORN | GDK_KEY_Thorn | GDK_KEY_ssharp | GDK_KEY_agrave | GDK_KEY_aacute
             | GDK_KEY_acircumflex | GDK_KEY_atilde | GDK_KEY_adiaeresis | GDK_KEY_aring | GDK_KEY_ae
             | GDK_KEY_ccedilla | GDK_KEY_egrave | GDK_KEY_eacute | GDK_KEY_ecircumflex | GDK_KEY_ediaeresis
             | GDK_KEY_igrave | GDK_KEY_iacute | GDK_KEY_icircumflex | GDK_KEY_idiaeresis | GDK_KEY_eth
             | GDK_KEY_ntilde | GDK_KEY_ograve | GDK_KEY_oacute | GDK_KEY_ocircumflex | GDK_KEY_otilde
             | GDK_KEY_odiaeresis | GDK_KEY_division | GDK_KEY_oslash | GDK_KEY_ooblique | GDK_KEY_ugrave
             | GDK_KEY_uacute | GDK_KEY_ucircumflex | GDK_KEY_udiaeresis | GDK_KEY_yacute | GDK_KEY_thorn
             | GDK_KEY_ydiaeresis | GDK_KEY_Aogonek | GDK_KEY_breve | GDK_KEY_Lstroke | GDK_KEY_Lcaron
             | GDK_KEY_Sacute | GDK_KEY_Scaron | GDK_KEY_Scedilla | GDK_KEY_Tcaron | GDK_KEY_Zacute
             | GDK_KEY_Zcaron | GDK_KEY_Zabovedot | GDK_KEY_aogonek | GDK_KEY_ogonek | GDK_KEY_lstroke
             | GDK_KEY_lcaron | GDK_KEY_sacute | GDK_KEY_caron | GDK_KEY_scaron | GDK_KEY_scedilla
             | GDK_KEY_tcaron | GDK_KEY_zacute | GDK_KEY_doubleacute | GDK_KEY_zcaron | GDK_KEY_zabovedot
             | GDK_KEY_Racute | GDK_KEY_Abreve | GDK_KEY_Lacute | GDK_KEY_Cacute | GDK_KEY_Ccaron
             | GDK_KEY_Eogonek | GDK_KEY_Ecaron | GDK_KEY_Dcaron | GDK_KEY_Dstroke | GDK_KEY_Nacute
             | GDK_KEY_Ncaron | GDK_KEY_Odoubleacute | GDK_KEY_Rcaron | GDK_KEY_Uring | GDK_KEY_Udoubleacute
             | GDK_KEY_Tcedilla | GDK_KEY_racute | GDK_KEY_abreve | GDK_KEY_lacute | GDK_KEY_cacute
             | GDK_KEY_ccaron | GDK_KEY_eogonek | GDK_KEY_ecaron | GDK_KEY_dcaron | GDK_KEY_dstroke
             | GDK_KEY_nacute | GDK_KEY_ncaron | GDK_KEY_odoubleacute | GDK_KEY_rcaron
             | GDK_KEY_uring | GDK_KEY_udoubleacute | GDK_KEY_tcedilla | GDK_KEY_abovedot
             | GDK_KEY_Hstroke | GDK_KEY_Hcircumflex | GDK_KEY_Iabovedot | GDK_KEY_Gbreve | GDK_KEY_Jcircumflex
             | GDK_KEY_hstroke | GDK_KEY_hcircumflex | GDK_KEY_idotless | GDK_KEY_gbreve
             | GDK_KEY_jcircumflex | GDK_KEY_Cabovedot | GDK_KEY_Ccircumflex | GDK_KEY_Gabovedot
             | GDK_KEY_Gcircumflex | GDK_KEY_Ubreve | GDK_KEY_Scircumflex | GDK_KEY_cabovedot | GDK_KEY_ccircumflex
             | GDK_KEY_gabovedot | GDK_KEY_gcircumflex | GDK_KEY_ubreve | GDK_KEY_scircumflex
             | GDK_KEY_kra | GDK_KEY_kappa | GDK_KEY_Rcedilla | GDK_KEY_Itilde | GDK_KEY_Lcedilla
             | GDK_KEY_Emacron | GDK_KEY_Gcedilla | GDK_KEY_Tslash | GDK_KEY_rcedilla | GDK_KEY_itilde
             | GDK_KEY_lcedilla | GDK_KEY_emacron | GDK_KEY_gcedilla | GDK_KEY_tslash | GDK_KEY_ENG
             | GDK_KEY_eng | GDK_KEY_Amacron | GDK_KEY_Iogonek | GDK_KEY_Eabovedot | GDK_KEY_Imacron
             | GDK_KEY_Ncedilla | GDK_KEY_Omacron | GDK_KEY_Kcedilla | GDK_KEY_Uogonek | GDK_KEY_Utilde
             | GDK_KEY_Umacron | GDK_KEY_amacron | GDK_KEY_iogonek | GDK_KEY_eabovedot | GDK_KEY_imacron
             | GDK_KEY_ncedilla | GDK_KEY_omacron | GDK_KEY_kcedilla | GDK_KEY_uogonek | GDK_KEY_utilde
             | GDK_KEY_umacron | GDK_KEY_Wcircumflex | GDK_KEY_wcircumflex | GDK_KEY_Ycircumflex | GDK_KEY_ycircumflex
             | GDK_KEY_Babovedot | GDK_KEY_babovedot | GDK_KEY_Dabovedot | GDK_KEY_dabovedot | GDK_KEY_Fabovedot
             | GDK_KEY_fabovedot | GDK_KEY_Mabovedot | GDK_KEY_mabovedot | GDK_KEY_Pabovedot | GDK_KEY_pabovedot
             | GDK_KEY_Sabovedot | GDK_KEY_sabovedot | GDK_KEY_Tabovedot | GDK_KEY_tabovedot | GDK_KEY_Wgrave
             | GDK_KEY_wgrave | GDK_KEY_Wacute | GDK_KEY_wacute | GDK_KEY_Wdiaeresis | GDK_KEY_wdiaeresis
             | GDK_KEY_Ygrave | GDK_KEY_ygrave | GDK_KEY_OE | GDK_KEY_oe | GDK_KEY_Ydiaeresis
             | GDK_KEY_overline | GDK_KEY_kana_fullstop | GDK_KEY_kana_openingbracket | GDK_KEY_kana_closingbracket | GDK_KEY_kana_comma
             | GDK_KEY_kana_conjunctive | GDK_KEY_kana_middledot | GDK_KEY_kana_WO | GDK_KEY_kana_a | GDK_KEY_kana_i
             | GDK_KEY_kana_u | GDK_KEY_kana_e | GDK_KEY_kana_o | GDK_KEY_kana_ya | GDK_KEY_kana_yu | GDK_KEY_kana_yo
             | GDK_KEY_kana_tsu | GDK_KEY_kana_tu | GDK_KEY_prolongedsound | GDK_KEY_kana_A | GDK_KEY_kana_I
             | GDK_KEY_kana_U | GDK_KEY_kana_E | GDK_KEY_kana_O | GDK_KEY_kana_KA | GDK_KEY_kana_KI
             | GDK_KEY_kana_KU | GDK_KEY_kana_KE | GDK_KEY_kana_KO | GDK_KEY_kana_SA | GDK_KEY_kana_SHI | GDK_KEY_kana_SU
             | GDK_KEY_kana_SE | GDK_KEY_kana_SO | GDK_KEY_kana_TA | GDK_KEY_kana_CHI | GDK_KEY_kana_TI
             | GDK_KEY_kana_TSU | GDK_KEY_kana_TU | GDK_KEY_kana_TE | GDK_KEY_kana_TO | GDK_KEY_kana_NA
             | GDK_KEY_kana_NI | GDK_KEY_kana_NU | GDK_KEY_kana_NE | GDK_KEY_kana_NO | GDK_KEY_kana_HA
             | GDK_KEY_kana_HI | GDK_KEY_kana_FU | GDK_KEY_kana_HU | GDK_KEY_kana_HE | GDK_KEY_kana_HO | GDK_KEY_kana_MA
             | GDK_KEY_kana_MI | GDK_KEY_kana_MU | GDK_KEY_kana_ME | GDK_KEY_kana_MO | GDK_KEY_kana_YA
             | GDK_KEY_kana_YU | GDK_KEY_kana_YO | GDK_KEY_kana_RA | GDK_KEY_kana_RI | GDK_KEY_kana_RU
             | GDK_KEY_kana_RE | GDK_KEY_kana_RO | GDK_KEY_kana_WA | GDK_KEY_kana_N | GDK_KEY_voicedsound
             | GDK_KEY_semivoicedsound | GDK_KEY_kana_switch | GDK_KEY_Farsi_0 | GDK_KEY_Farsi_1 | GDK_KEY_Farsi_2
             | GDK_KEY_Farsi_3 | GDK_KEY_Farsi_4 | GDK_KEY_Farsi_5 | GDK_KEY_Farsi_6 | GDK_KEY_Farsi_7
             | GDK_KEY_Farsi_8 | GDK_KEY_Farsi_9 | GDK_KEY_Arabic_percent | GDK_KEY_Arabic_superscript_alef | GDK_KEY_Arabic_tteh
             | GDK_KEY_Arabic_peh | GDK_KEY_Arabic_tcheh | GDK_KEY_Arabic_ddal | GDK_KEY_Arabic_rreh | GDK_KEY_Arabic_comma
             | GDK_KEY_Arabic_fullstop | GDK_KEY_Arabic_0 | GDK_KEY_Arabic_1 | GDK_KEY_Arabic_2 | GDK_KEY_Arabic_3
             | GDK_KEY_Arabic_4 | GDK_KEY_Arabic_5 | GDK_KEY_Arabic_6 | GDK_KEY_Arabic_7 | GDK_KEY_Arabic_8
             | GDK_KEY_Arabic_9 | GDK_KEY_Arabic_semicolon | GDK_KEY_Arabic_question_mark | GDK_KEY_Arabic_hamza | GDK_KEY_Arabic_maddaonalef
             | GDK_KEY_Arabic_hamzaonalef | GDK_KEY_Arabic_hamzaonwaw | GDK_KEY_Arabic_hamzaunderalef | GDK_KEY_Arabic_hamzaonyeh
             | GDK_KEY_Arabic_alef | GDK_KEY_Arabic_beh | GDK_KEY_Arabic_tehmarbuta | GDK_KEY_Arabic_teh
             | GDK_KEY_Arabic_theh | GDK_KEY_Arabic_jeem | GDK_KEY_Arabic_hah | GDK_KEY_Arabic_khah
             | GDK_KEY_Arabic_dal | GDK_KEY_Arabic_thal | GDK_KEY_Arabic_ra | GDK_KEY_Arabic_zain | GDK_KEY_Arabic_seen
             | GDK_KEY_Arabic_sheen | GDK_KEY_Arabic_sad | GDK_KEY_Arabic_dad | GDK_KEY_Arabic_tah | GDK_KEY_Arabic_zah
             | GDK_KEY_Arabic_ain | GDK_KEY_Arabic_ghain | GDK_KEY_Arabic_tatweel | GDK_KEY_Arabic_feh | GDK_KEY_Arabic_qaf
             | GDK_KEY_Arabic_kaf | GDK_KEY_Arabic_lam | GDK_KEY_Arabic_meem | GDK_KEY_Arabic_noon
             | GDK_KEY_Arabic_ha | GDK_KEY_Arabic_heh | GDK_KEY_Arabic_waw | GDK_KEY_Arabic_alefmaksura
             | GDK_KEY_Arabic_yeh | GDK_KEY_Arabic_fathatan | GDK_KEY_Arabic_dammatan | GDK_KEY_Arabic_kasratan
             | GDK_KEY_Arabic_fatha | GDK_KEY_Arabic_damma | GDK_KEY_Arabic_kasra | GDK_KEY_Arabic_shadda
             | GDK_KEY_Arabic_sukun | GDK_KEY_Arabic_madda_above | GDK_KEY_Arabic_hamza_above | GDK_KEY_Arabic_hamza_below
             | GDK_KEY_Arabic_jeh | GDK_KEY_Arabic_veh | GDK_KEY_Arabic_keheh | GDK_KEY_Arabic_gaf
             | GDK_KEY_Arabic_noon_ghunna | GDK_KEY_Arabic_heh_doachashmee | GDK_KEY_Farsi_yeh | GDK_KEY_Arabic_farsi_yeh
             | GDK_KEY_Arabic_yeh_baree | GDK_KEY_Arabic_heh_goal | GDK_KEY_Arabic_switch | GDK_KEY_Cyrillic_GHE_bar
             | GDK_KEY_Cyrillic_ghe_bar | GDK_KEY_Cyrillic_ZHE_descender | GDK_KEY_Cyrillic_zhe_descender | GDK_KEY_Cyrillic_KA_descender
             | GDK_KEY_Cyrillic_ka_descender | GDK_KEY_Cyrillic_KA_vertstroke | GDK_KEY_Cyrillic_ka_vertstroke
             | GDK_KEY_Cyrillic_EN_descender | GDK_KEY_Cyrillic_en_descender | GDK_KEY_Cyrillic_U_straight | GDK_KEY_Cyrillic_u_straight
             | GDK_KEY_Cyrillic_U_straight_bar | GDK_KEY_Cyrillic_u_straight_bar | GDK_KEY_Cyrillic_HA_descender | GDK_KEY_Cyrillic_ha_descender
             | GDK_KEY_Cyrillic_CHE_descender | GDK_KEY_Cyrillic_che_descender | GDK_KEY_Cyrillic_CHE_vertstroke | GDK_KEY_Cyrillic_che_vertstroke
             | GDK_KEY_Cyrillic_SHHA | GDK_KEY_Cyrillic_shha | GDK_KEY_Cyrillic_SCHWA | GDK_KEY_Cyrillic_schwa
             | GDK_KEY_Cyrillic_I_macron | GDK_KEY_Cyrillic_i_macron | GDK_KEY_Cyrillic_O_bar | GDK_KEY_Cyrillic_o_bar
             | GDK_KEY_Cyrillic_U_macron | GDK_KEY_Cyrillic_u_macron | GDK_KEY_Serbian_dje | GDK_KEY_Macedonia_gje
             | GDK_KEY_Cyrillic_io | GDK_KEY_Ukrainian_ie | GDK_KEY_Ukranian_je | GDK_KEY_Macedonia_dse
             | GDK_KEY_Ukrainian_i | GDK_KEY_Ukranian_i | GDK_KEY_Ukrainian_yi | GDK_KEY_Ukranian_yi
             | GDK_KEY_Cyrillic_je | GDK_KEY_Serbian_je | GDK_KEY_Cyrillic_lje | GDK_KEY_Serbian_lje
             | GDK_KEY_Cyrillic_nje | GDK_KEY_Serbian_nje | GDK_KEY_Serbian_tshe | GDK_KEY_Macedonia_kje
             | GDK_KEY_Ukrainian_ghe_with_upturn | GDK_KEY_Byelorussian_shortu | GDK_KEY_Cyrillic_dzhe | GDK_KEY_Serbian_dze
             | GDK_KEY_numerosign | GDK_KEY_Serbian_DJE | GDK_KEY_Macedonia_GJE | GDK_KEY_Cyrillic_IO
             | GDK_KEY_Ukrainian_IE | GDK_KEY_Ukranian_JE | GDK_KEY_Macedonia_DSE | GDK_KEY_Ukrainian_I
             | GDK_KEY_Ukranian_I | GDK_KEY_Ukrainian_YI | GDK_KEY_Ukranian_YI | GDK_KEY_Cyrillic_JE
             | GDK_KEY_Serbian_JE | GDK_KEY_Cyrillic_LJE | GDK_KEY_Serbian_LJE | GDK_KEY_Cyrillic_NJE | GDK_KEY_Serbian_NJE
             | GDK_KEY_Serbian_TSHE | GDK_KEY_Macedonia_KJE | GDK_KEY_Ukrainian_GHE_WITH_UPTURN | GDK_KEY_Byelorussian_SHORTU
             | GDK_KEY_Cyrillic_DZHE | GDK_KEY_Serbian_DZE | GDK_KEY_Cyrillic_yu | GDK_KEY_Cyrillic_a | GDK_KEY_Cyrillic_be
             | GDK_KEY_Cyrillic_tse | GDK_KEY_Cyrillic_de | GDK_KEY_Cyrillic_ie | GDK_KEY_Cyrillic_ef | GDK_KEY_Cyrillic_ghe
             | GDK_KEY_Cyrillic_ha | GDK_KEY_Cyrillic_i | GDK_KEY_Cyrillic_shorti | GDK_KEY_Cyrillic_ka | GDK_KEY_Cyrillic_el
             | GDK_KEY_Cyrillic_em | GDK_KEY_Cyrillic_en | GDK_KEY_Cyrillic_o | GDK_KEY_Cyrillic_pe | GDK_KEY_Cyrillic_ya
             | GDK_KEY_Cyrillic_er | GDK_KEY_Cyrillic_es | GDK_KEY_Cyrillic_te | GDK_KEY_Cyrillic_u | GDK_KEY_Cyrillic_zhe
             | GDK_KEY_Cyrillic_ve | GDK_KEY_Cyrillic_softsign | GDK_KEY_Cyrillic_yeru | GDK_KEY_Cyrillic_ze | GDK_KEY_Cyrillic_sha
             | GDK_KEY_Cyrillic_e | GDK_KEY_Cyrillic_shcha | GDK_KEY_Cyrillic_che | GDK_KEY_Cyrillic_hardsign | GDK_KEY_Cyrillic_YU
             | GDK_KEY_Cyrillic_A | GDK_KEY_Cyrillic_BE | GDK_KEY_Cyrillic_TSE | GDK_KEY_Cyrillic_DE | GDK_KEY_Cyrillic_IE
             | GDK_KEY_Cyrillic_EF | GDK_KEY_Cyrillic_GHE | GDK_KEY_Cyrillic_HA | GDK_KEY_Cyrillic_I | GDK_KEY_Cyrillic_SHORTI
             | GDK_KEY_Cyrillic_KA | GDK_KEY_Cyrillic_EL | GDK_KEY_Cyrillic_EM | GDK_KEY_Cyrillic_EN | GDK_KEY_Cyrillic_O
             | GDK_KEY_Cyrillic_PE | GDK_KEY_Cyrillic_YA | GDK_KEY_Cyrillic_ER | GDK_KEY_Cyrillic_ES | GDK_KEY_Cyrillic_TE
             | GDK_KEY_Cyrillic_U | GDK_KEY_Cyrillic_ZHE | GDK_KEY_Cyrillic_VE | GDK_KEY_Cyrillic_SOFTSIGN | GDK_KEY_Cyrillic_YERU
             | GDK_KEY_Cyrillic_ZE | GDK_KEY_Cyrillic_SHA | GDK_KEY_Cyrillic_E | GDK_KEY_Cyrillic_SHCHA | GDK_KEY_Cyrillic_CHE | GDK_KEY_Cyrillic_HARDSIGN
             | GDK_KEY_Greek_ALPHAaccent | GDK_KEY_Greek_EPSILONaccent | GDK_KEY_Greek_ETAaccent | GDK_KEY_Greek_IOTAaccent | GDK_KEY_Greek_IOTAdieresis
             | GDK_KEY_Greek_IOTAdiaeresis | GDK_KEY_Greek_OMICRONaccent | GDK_KEY_Greek_UPSILONaccent | GDK_KEY_Greek_UPSILONdieresis
             | GDK_KEY_Greek_OMEGAaccent | GDK_KEY_Greek_accentdieresis | GDK_KEY_Greek_horizbar | GDK_KEY_Greek_alphaaccent
             | GDK_KEY_Greek_epsilonaccent | GDK_KEY_Greek_etaaccent | GDK_KEY_Greek_iotaaccent | GDK_KEY_Greek_iotadieresis
             | GDK_KEY_Greek_iotaaccentdieresis | GDK_KEY_Greek_omicronaccent | GDK_KEY_Greek_upsilonaccent
             | GDK_KEY_Greek_upsilondieresis | GDK_KEY_Greek_upsilonaccentdieresis | GDK_KEY_Greek_omegaaccent | GDK_KEY_Greek_ALPHA
             | GDK_KEY_Greek_BETA | GDK_KEY_Greek_GAMMA | GDK_KEY_Greek_DELTA | GDK_KEY_Greek_EPSILON
             | GDK_KEY_Greek_ZETA | GDK_KEY_Greek_ETA | GDK_KEY_Greek_THETA | GDK_KEY_Greek_IOTA | GDK_KEY_Greek_KAPPA
             | GDK_KEY_Greek_LAMDA | GDK_KEY_Greek_LAMBDA | GDK_KEY_Greek_MU | GDK_KEY_Greek_NU | GDK_KEY_Greek_XI
             | GDK_KEY_Greek_OMICRON | GDK_KEY_Greek_PI | GDK_KEY_Greek_RHO | GDK_KEY_Greek_SIGMA | GDK_KEY_Greek_TAU
             | GDK_KEY_Greek_UPSILON | GDK_KEY_Greek_PHI | GDK_KEY_Greek_CHI | GDK_KEY_Greek_PSI
             | GDK_KEY_Greek_OMEGA | GDK_KEY_Greek_alpha | GDK_KEY_Greek_beta | GDK_KEY_Greek_gamma | GDK_KEY_Greek_delta
             | GDK_KEY_Greek_epsilon | GDK_KEY_Greek_zeta | GDK_KEY_Greek_eta | GDK_KEY_Greek_theta | GDK_KEY_Greek_iota
             | GDK_KEY_Greek_kappa | GDK_KEY_Greek_lamda | GDK_KEY_Greek_lambda | GDK_KEY_Greek_mu
             | GDK_KEY_Greek_nu | GDK_KEY_Greek_xi | GDK_KEY_Greek_omicron | GDK_KEY_Greek_pi | GDK_KEY_Greek_rho
             | GDK_KEY_Greek_sigma | GDK_KEY_Greek_finalsmallsigma | GDK_KEY_Greek_tau | GDK_KEY_Greek_upsilon | GDK_KEY_Greek_phi
             | GDK_KEY_Greek_chi | GDK_KEY_Greek_psi | GDK_KEY_Greek_omega | GDK_KEY_Greek_switch | GDK_KEY_leftradical
             | GDK_KEY_topleftradical | GDK_KEY_horizconnector | GDK_KEY_topintegral | GDK_KEY_botintegral | GDK_KEY_vertconnector
             | GDK_KEY_topleftsqbracket | GDK_KEY_botleftsqbracket | GDK_KEY_toprightsqbracket | GDK_KEY_botrightsqbracket | GDK_KEY_topleftparens
             | GDK_KEY_botleftparens | GDK_KEY_toprightparens | GDK_KEY_botrightparens | GDK_KEY_leftmiddlecurlybrace
             | GDK_KEY_rightmiddlecurlybrace | GDK_KEY_topleftsummation | GDK_KEY_botleftsummation | GDK_KEY_topvertsummationconnector
             | GDK_KEY_botvertsummationconnector | GDK_KEY_toprightsummation | GDK_KEY_botrightsummation | GDK_KEY_rightmiddlesummation
             | GDK_KEY_lessthanequal | GDK_KEY_notequal | GDK_KEY_greaterthanequal | GDK_KEY_integral
             | GDK_KEY_therefore | GDK_KEY_variation | GDK_KEY_infinity | GDK_KEY_nabla | GDK_KEY_approximate
             | GDK_KEY_similarequal | GDK_KEY_ifonlyif | GDK_KEY_implies | GDK_KEY_identical | GDK_KEY_radical
             | GDK_KEY_includedin | GDK_KEY_includes | GDK_KEY_intersection | GDK_KEY_union | GDK_KEY_logicaland | GDK_KEY_logicalor
             | GDK_KEY_partialderivative | GDK_KEY_function | GDK_KEY_leftarrow | GDK_KEY_uparrow | GDK_KEY_rightarrow | GDK_KEY_downarrow
             | GDK_KEY_blank | GDK_KEY_soliddiamond | GDK_KEY_checkerboard | GDK_KEY_ht | GDK_KEY_ff | GDK_KEY_cr
             | GDK_KEY_lf | GDK_KEY_nl | GDK_KEY_vt | GDK_KEY_lowrightcorner | GDK_KEY_uprightcorner
             | GDK_KEY_upleftcorner | GDK_KEY_lowleftcorner | GDK_KEY_crossinglines | GDK_KEY_horizlinescan1 | GDK_KEY_horizlinescan3
             | GDK_KEY_horizlinescan5 | GDK_KEY_horizlinescan7 | GDK_KEY_horizlinescan9 | GDK_KEY_leftt | GDK_KEY_rightt
             | GDK_KEY_bott | GDK_KEY_topt | GDK_KEY_vertbar | GDK_KEY_emspace | GDK_KEY_enspace | GDK_KEY_em3space | GDK_KEY_em4space
             | GDK_KEY_digitspace | GDK_KEY_punctspace | GDK_KEY_thinspace | GDK_KEY_hairspace | GDK_KEY_emdash | GDK_KEY_endash
             | GDK_KEY_signifblank | GDK_KEY_ellipsis | GDK_KEY_doubbaselinedot | GDK_KEY_onethird | GDK_KEY_twothirds | GDK_KEY_onefifth
             | GDK_KEY_twofifths | GDK_KEY_threefifths | GDK_KEY_fourfifths | GDK_KEY_onesixth | GDK_KEY_fivesixths | GDK_KEY_careof
             | GDK_KEY_figdash | GDK_KEY_leftanglebracket | GDK_KEY_decimalpoint | GDK_KEY_rightanglebracket | GDK_KEY_marker
             | GDK_KEY_oneeighth | GDK_KEY_threeeighths | GDK_KEY_fiveeighths | GDK_KEY_seveneighths | GDK_KEY_trademark
             | GDK_KEY_signaturemark | GDK_KEY_trademarkincircle | GDK_KEY_leftopentriangle | GDK_KEY_rightopentriangle | GDK_KEY_emopencircle
             | GDK_KEY_emopenrectangle | GDK_KEY_leftsinglequotemark | GDK_KEY_rightsinglequotemark | GDK_KEY_leftdoublequotemark | GDK_KEY_rightdoublequotemark
             | GDK_KEY_prescription | GDK_KEY_permille | GDK_KEY_minutes | GDK_KEY_seconds | GDK_KEY_latincross
             | GDK_KEY_hexagram | GDK_KEY_filledrectbullet | GDK_KEY_filledlefttribullet | GDK_KEY_filledrighttribullet
             | GDK_KEY_emfilledcircle | GDK_KEY_emfilledrect | GDK_KEY_enopencircbullet | GDK_KEY_enopensquarebullet | GDK_KEY_openrectbullet
             | GDK_KEY_opentribulletup | GDK_KEY_opentribulletdown | GDK_KEY_openstar | GDK_KEY_enfilledcircbullet | GDK_KEY_enfilledsqbullet
             | GDK_KEY_filledtribulletup | GDK_KEY_filledtribulletdown | GDK_KEY_leftpointer | GDK_KEY_rightpointer | GDK_KEY_club | GDK_KEY_diamond
             | GDK_KEY_heart | GDK_KEY_maltesecross | GDK_KEY_dagger | GDK_KEY_doubledagger | GDK_KEY_checkmark | GDK_KEY_ballotcross
             | GDK_KEY_musicalsharp | GDK_KEY_musicalflat | GDK_KEY_malesymbol | GDK_KEY_femalesymbol | GDK_KEY_telephone | GDK_KEY_telephonerecorder | GDK_KEY_phonographcopyright
             | GDK_KEY_caret | GDK_KEY_singlelowquotemark | GDK_KEY_doublelowquotemark | GDK_KEY_cursor | GDK_KEY_leftcaret | GDK_KEY_rightcaret
             | GDK_KEY_downcaret | GDK_KEY_upcaret | GDK_KEY_overbar | GDK_KEY_downtack | GDK_KEY_upshoe | GDK_KEY_downstile
             | GDK_KEY_underbar | GDK_KEY_jot | GDK_KEY_quad | GDK_KEY_uptack | GDK_KEY_circle | GDK_KEY_upstile
             | GDK_KEY_downshoe | GDK_KEY_rightshoe | GDK_KEY_leftshoe | GDK_KEY_lefttack | GDK_KEY_righttack | GDK_KEY_hebrew_doublelowline
             | GDK_KEY_hebrew_aleph | GDK_KEY_hebrew_bet | GDK_KEY_hebrew_beth | GDK_KEY_hebrew_gimel | GDK_KEY_hebrew_gimmel
             | GDK_KEY_hebrew_dalet | GDK_KEY_hebrew_daleth | GDK_KEY_hebrew_he | GDK_KEY_hebrew_waw | GDK_KEY_hebrew_zain | GDK_KEY_hebrew_zayin
             | GDK_KEY_hebrew_chet | GDK_KEY_hebrew_het | GDK_KEY_hebrew_tet | GDK_KEY_hebrew_teth | GDK_KEY_hebrew_yod | GDK_KEY_hebrew_finalkaph
             | GDK_KEY_hebrew_kaph | GDK_KEY_hebrew_lamed | GDK_KEY_hebrew_finalmem | GDK_KEY_hebrew_mem | GDK_KEY_hebrew_finalnun
             | GDK_KEY_hebrew_nun | GDK_KEY_hebrew_samech | GDK_KEY_hebrew_samekh | GDK_KEY_hebrew_ayin | GDK_KEY_hebrew_finalpe
             | GDK_KEY_hebrew_pe | GDK_KEY_hebrew_finalzade | GDK_KEY_hebrew_finalzadi | GDK_KEY_hebrew_zade | GDK_KEY_hebrew_zadi | GDK_KEY_hebrew_qoph | GDK_KEY_hebrew_kuf
             | GDK_KEY_hebrew_resh | GDK_KEY_hebrew_shin | GDK_KEY_hebrew_taw | GDK_KEY_hebrew_taf | GDK_KEY_Hebrew_switch
             | GDK_KEY_Thai_kokai | GDK_KEY_Thai_khokhai | GDK_KEY_Thai_khokhuat | GDK_KEY_Thai_khokhwai
             | GDK_KEY_Thai_khokhon | GDK_KEY_Thai_khorakhang | GDK_KEY_Thai_ngongu | GDK_KEY_Thai_chochan
             | GDK_KEY_Thai_choching | GDK_KEY_Thai_chochang | GDK_KEY_Thai_soso | GDK_KEY_Thai_chochoe | GDK_KEY_Thai_yoying
             | GDK_KEY_Thai_dochada | GDK_KEY_Thai_topatak | GDK_KEY_Thai_thothan | GDK_KEY_Thai_thonangmontho | GDK_KEY_Thai_thophuthao
             | GDK_KEY_Thai_nonen | GDK_KEY_Thai_dodek
             | GDK_KEY_Thai_totao | GDK_KEY_Thai_thothung | GDK_KEY_Thai_thothahan
             | GDK_KEY_Thai_thothong | GDK_KEY_Thai_nonu
             | GDK_KEY_Thai_bobaimai | GDK_KEY_Thai_popla | GDK_KEY_Thai_phophung | GDK_KEY_Thai_fofa
             | GDK_KEY_Thai_phophan | GDK_KEY_Thai_fofan | GDK_KEY_Thai_phosamphao | GDK_KEY_Thai_moma | GDK_KEY_Thai_yoyak
             | GDK_KEY_Thai_rorua | GDK_KEY_Thai_ru | GDK_KEY_Thai_loling | GDK_KEY_Thai_lu
             | GDK_KEY_Thai_wowaen | GDK_KEY_Thai_sosala | GDK_KEY_Thai_sorusi | GDK_KEY_Thai_sosua | GDK_KEY_Thai_hohip
             | GDK_KEY_Thai_lochula | GDK_KEY_Thai_oang | GDK_KEY_Thai_honokhuk | GDK_KEY_Thai_paiyannoi | GDK_KEY_Thai_saraa
             | GDK_KEY_Thai_maihanakat | GDK_KEY_Thai_saraaa | GDK_KEY_Thai_saraam | GDK_KEY_Thai_sarai | GDK_KEY_Thai_saraii
             | GDK_KEY_Thai_saraue | GDK_KEY_Thai_sarauee | GDK_KEY_Thai_sarau | GDK_KEY_Thai_sarauu | GDK_KEY_Thai_phinthu
             | GDK_KEY_Thai_maihanakat_maitho | GDK_KEY_Thai_baht | GDK_KEY_Thai_sarae | GDK_KEY_Thai_saraae | GDK_KEY_Thai_sarao
             | GDK_KEY_Thai_saraaimaimuan | GDK_KEY_Thai_saraaimaimalai | GDK_KEY_Thai_lakkhangyao | GDK_KEY_Thai_maiyamok
             | GDK_KEY_Thai_maitaikhu | GDK_KEY_Thai_maiek | GDK_KEY_Thai_maitho | GDK_KEY_Thai_maitri | GDK_KEY_Thai_maichattawa
             | GDK_KEY_Thai_thanthakhat | GDK_KEY_Thai_nikhahit | GDK_KEY_Thai_leksun | GDK_KEY_Thai_leknung | GDK_KEY_Thai_leksong
             | GDK_KEY_Thai_leksam | GDK_KEY_Thai_leksi | GDK_KEY_Thai_lekha | GDK_KEY_Thai_lekhok | GDK_KEY_Thai_lekchet
             | GDK_KEY_Thai_lekpaet | GDK_KEY_Thai_lekkao | GDK_KEY_Hangul | GDK_KEY_Hangul_Start | GDK_KEY_Hangul_End
             | GDK_KEY_Hangul_Hanja | GDK_KEY_Hangul_Jamo | GDK_KEY_Hangul_Romaja | GDK_KEY_Hangul_Codeinput | GDK_KEY_Hangul_Jeonja
             | GDK_KEY_Hangul_Banja | GDK_KEY_Hangul_PreHanja | GDK_KEY_Hangul_PostHanja | GDK_KEY_Hangul_SingleCandidate | GDK_KEY_Hangul_MultipleCandidate
             | GDK_KEY_Hangul_PreviousCandidate | GDK_KEY_Hangul_Special | GDK_KEY_Hangul_switch | GDK_KEY_Hangul_Kiyeog
             | GDK_KEY_Hangul_SsangKiyeog | GDK_KEY_Hangul_KiyeogSios | GDK_KEY_Hangul_Nieun | GDK_KEY_Hangul_NieunJieuj | GDK_KEY_Hangul_NieunHieuh
             | GDK_KEY_Hangul_Dikeud | GDK_KEY_Hangul_SsangDikeud | GDK_KEY_Hangul_Rieul | GDK_KEY_Hangul_RieulKiyeog | GDK_KEY_Hangul_RieulMieum
             | GDK_KEY_Hangul_RieulPieub | GDK_KEY_Hangul_RieulSios | GDK_KEY_Hangul_RieulTieut | GDK_KEY_Hangul_RieulPhieuf | GDK_KEY_Hangul_RieulHieuh
             | GDK_KEY_Hangul_Mieum | GDK_KEY_Hangul_Pieub | GDK_KEY_Hangul_SsangPieub | GDK_KEY_Hangul_PieubSios
             | GDK_KEY_Hangul_Sios | GDK_KEY_Hangul_SsangSios | GDK_KEY_Hangul_Ieung | GDK_KEY_Hangul_Jieuj
             | GDK_KEY_Hangul_SsangJieuj | GDK_KEY_Hangul_Cieuc | GDK_KEY_Hangul_Khieuq | GDK_KEY_Hangul_Tieut | GDK_KEY_Hangul_Phieuf
             | GDK_KEY_Hangul_Hieuh | GDK_KEY_Hangul_A | GDK_KEY_Hangul_AE | GDK_KEY_Hangul_YA | GDK_KEY_Hangul_YAE
             | GDK_KEY_Hangul_EO | GDK_KEY_Hangul_E | GDK_KEY_Hangul_YEO | GDK_KEY_Hangul_YE
             | GDK_KEY_Hangul_O | GDK_KEY_Hangul_WA | GDK_KEY_Hangul_WAE | GDK_KEY_Hangul_OE | GDK_KEY_Hangul_YO
             | GDK_KEY_Hangul_U | GDK_KEY_Hangul_WEO | GDK_KEY_Hangul_WE | GDK_KEY_Hangul_WI | GDK_KEY_Hangul_YU
             | GDK_KEY_Hangul_EU | GDK_KEY_Hangul_YI | GDK_KEY_Hangul_I | GDK_KEY_Hangul_J_Kiyeog | GDK_KEY_Hangul_J_SsangKiyeog
             | GDK_KEY_Hangul_J_KiyeogSios | GDK_KEY_Hangul_J_Nieun | GDK_KEY_Hangul_J_NieunJieuj | GDK_KEY_Hangul_J_NieunHieuh
             | GDK_KEY_Hangul_J_Dikeud | GDK_KEY_Hangul_J_Rieul | GDK_KEY_Hangul_J_RieulKiyeog | GDK_KEY_Hangul_J_RieulMieum | GDK_KEY_Hangul_J_RieulPieub
             | GDK_KEY_Hangul_J_RieulSios | GDK_KEY_Hangul_J_RieulTieut | GDK_KEY_Hangul_J_RieulPhieuf | GDK_KEY_Hangul_J_RieulHieuh
             | GDK_KEY_Hangul_J_Mieum | GDK_KEY_Hangul_J_Pieub | GDK_KEY_Hangul_J_PieubSios | GDK_KEY_Hangul_J_Sios | GDK_KEY_Hangul_J_SsangSios
             | GDK_KEY_Hangul_J_Ieung | GDK_KEY_Hangul_J_Jieuj | GDK_KEY_Hangul_J_Cieuc | GDK_KEY_Hangul_J_Khieuq | GDK_KEY_Hangul_J_Tieut
             | GDK_KEY_Hangul_J_Phieuf | GDK_KEY_Hangul_J_Hieuh | GDK_KEY_Hangul_RieulYeorinHieuh | GDK_KEY_Hangul_SunkyeongeumMieum | GDK_KEY_Hangul_SunkyeongeumPieub
             | GDK_KEY_Hangul_PanSios | GDK_KEY_Hangul_KkogjiDalrinIeung | GDK_KEY_Hangul_SunkyeongeumPhieuf | GDK_KEY_Hangul_YeorinHieuh | GDK_KEY_Hangul_AraeA
             | GDK_KEY_Hangul_AraeAE | GDK_KEY_Hangul_J_PanSios | GDK_KEY_Hangul_J_KkogjiDalrinIeung | GDK_KEY_Hangul_J_YeorinHieuh
             | GDK_KEY_Korean_Won | GDK_KEY_Armenian_ligature_ew | GDK_KEY_Armenian_full_stop | GDK_KEY_Armenian_verjaket | GDK_KEY_Armenian_separation_mark
             | GDK_KEY_Armenian_but | GDK_KEY_Armenian_hyphen | GDK_KEY_Armenian_yentamna | GDK_KEY_Armenian_exclam | GDK_KEY_Armenian_amanak
             | GDK_KEY_Armenian_accent | GDK_KEY_Armenian_shesht | GDK_KEY_Armenian_question | GDK_KEY_Armenian_paruyk | GDK_KEY_Armenian_AYB
             | GDK_KEY_Armenian_ayb | GDK_KEY_Armenian_BEN | GDK_KEY_Armenian_ben | GDK_KEY_Armenian_GIM | GDK_KEY_Armenian_gim
             | GDK_KEY_Armenian_DA | GDK_KEY_Armenian_da | GDK_KEY_Armenian_YECH | GDK_KEY_Armenian_yech | GDK_KEY_Armenian_ZA
             | GDK_KEY_Armenian_za | GDK_KEY_Armenian_E | GDK_KEY_Armenian_e | GDK_KEY_Armenian_AT | GDK_KEY_Armenian_at | GDK_KEY_Armenian_TO
             | GDK_KEY_Armenian_to | GDK_KEY_Armenian_ZHE | GDK_KEY_Armenian_zhe | GDK_KEY_Armenian_INI | GDK_KEY_Armenian_ini | GDK_KEY_Armenian_LYUN
             | GDK_KEY_Armenian_lyun | GDK_KEY_Armenian_KHE | GDK_KEY_Armenian_khe | GDK_KEY_Armenian_TSA | GDK_KEY_Armenian_tsa | GDK_KEY_Armenian_KEN
             | GDK_KEY_Armenian_ken | GDK_KEY_Armenian_HO | GDK_KEY_Armenian_ho | GDK_KEY_Armenian_DZA | GDK_KEY_Armenian_dza | GDK_KEY_Armenian_GHAT
             | GDK_KEY_Armenian_ghat | GDK_KEY_Armenian_TCHE | GDK_KEY_Armenian_tche | GDK_KEY_Armenian_MEN | GDK_KEY_Armenian_men | GDK_KEY_Armenian_HI
             | GDK_KEY_Armenian_hi | GDK_KEY_Armenian_NU | GDK_KEY_Armenian_nu | GDK_KEY_Armenian_SHA | GDK_KEY_Armenian_sha
             | GDK_KEY_Armenian_VO | GDK_KEY_Armenian_vo | GDK_KEY_Armenian_CHA | GDK_KEY_Armenian_cha | GDK_KEY_Armenian_PE | GDK_KEY_Armenian_pe
             | GDK_KEY_Armenian_JE | GDK_KEY_Armenian_je | GDK_KEY_Armenian_RA | GDK_KEY_Armenian_ra | GDK_KEY_Armenian_SE | GDK_KEY_Armenian_se
             | GDK_KEY_Armenian_VEV | GDK_KEY_Armenian_vev | GDK_KEY_Armenian_TYUN | GDK_KEY_Armenian_tyun | GDK_KEY_Armenian_RE | GDK_KEY_Armenian_re
             | GDK_KEY_Armenian_TSO | GDK_KEY_Armenian_tso
             | GDK_KEY_Armenian_VYUN
             | GDK_KEY_Armenian_vyun
             | GDK_KEY_Armenian_PYUR
             | GDK_KEY_Armenian_pyur
             | GDK_KEY_Armenian_KE
             | GDK_KEY_Armenian_ke
             | GDK_KEY_Armenian_O
             | GDK_KEY_Armenian_o
             | GDK_KEY_Armenian_FE
             | GDK_KEY_Armenian_fe
             | GDK_KEY_Armenian_apostrophe
             | GDK_KEY_Georgian_an
             | GDK_KEY_Georgian_ban
             | GDK_KEY_Georgian_gan
             | GDK_KEY_Georgian_don
             | GDK_KEY_Georgian_en
             | GDK_KEY_Georgian_vin
             | GDK_KEY_Georgian_zen
             | GDK_KEY_Georgian_tan
             | GDK_KEY_Georgian_in
             | GDK_KEY_Georgian_kan
             | GDK_KEY_Georgian_las
             | GDK_KEY_Georgian_man
             | GDK_KEY_Georgian_nar
             | GDK_KEY_Georgian_on
             | GDK_KEY_Georgian_par
             | GDK_KEY_Georgian_zhar
             | GDK_KEY_Georgian_rae
             | GDK_KEY_Georgian_san
             | GDK_KEY_Georgian_tar
             | GDK_KEY_Georgian_un
             | GDK_KEY_Georgian_phar
             | GDK_KEY_Georgian_khar
             | GDK_KEY_Georgian_ghan
             | GDK_KEY_Georgian_qar
             | GDK_KEY_Georgian_shin
             | GDK_KEY_Georgian_chin
             | GDK_KEY_Georgian_can
             | GDK_KEY_Georgian_jil
             | GDK_KEY_Georgian_cil
             | GDK_KEY_Georgian_char
             | GDK_KEY_Georgian_xan
             | GDK_KEY_Georgian_jhan
             | GDK_KEY_Georgian_hae
             | GDK_KEY_Georgian_he
             | GDK_KEY_Georgian_hie
             | GDK_KEY_Georgian_we
             | GDK_KEY_Georgian_har
             | GDK_KEY_Georgian_hoe
             | GDK_KEY_Georgian_fi
             | GDK_KEY_Xabovedot
             | GDK_KEY_Ibreve
             | GDK_KEY_Zstroke
             | GDK_KEY_Gcaron
             | GDK_KEY_Ocaron
             | GDK_KEY_Obarred
             | GDK_KEY_xabovedot
             | GDK_KEY_ibreve
             | GDK_KEY_zstroke
             | GDK_KEY_gcaron
             | GDK_KEY_ocaron
             | GDK_KEY_obarred
             | GDK_KEY_SCHWA
             | GDK_KEY_schwa
             | GDK_KEY_EZH
             | GDK_KEY_ezh
             | GDK_KEY_Lbelowdot
             | GDK_KEY_lbelowdot
             | GDK_KEY_Abelowdot
             | GDK_KEY_abelowdot
             | GDK_KEY_Ahook
             | GDK_KEY_ahook
             | GDK_KEY_Acircumflexacute
             | GDK_KEY_acircumflexacute
             | GDK_KEY_Acircumflexgrave
             | GDK_KEY_acircumflexgrave
             | GDK_KEY_Acircumflexhook
             | GDK_KEY_acircumflexhook
             | GDK_KEY_Acircumflextilde
             | GDK_KEY_acircumflextilde
             | GDK_KEY_Acircumflexbelowdot
             | GDK_KEY_acircumflexbelowdot
             | GDK_KEY_Abreveacute
             | GDK_KEY_abreveacute
             | GDK_KEY_Abrevegrave
             | GDK_KEY_abrevegrave
             | GDK_KEY_Abrevehook
             | GDK_KEY_abrevehook
             | GDK_KEY_Abrevetilde
             | GDK_KEY_abrevetilde
             | GDK_KEY_Abrevebelowdot
             | GDK_KEY_abrevebelowdot
             | GDK_KEY_Ebelowdot
             | GDK_KEY_ebelowdot
             | GDK_KEY_Ehook
             | GDK_KEY_ehook
             | GDK_KEY_Etilde
             | GDK_KEY_etilde
             | GDK_KEY_Ecircumflexacute
             | GDK_KEY_ecircumflexacute
             | GDK_KEY_Ecircumflexgrave
             | GDK_KEY_ecircumflexgrave
             | GDK_KEY_Ecircumflexhook
             | GDK_KEY_ecircumflexhook
             | GDK_KEY_Ecircumflextilde
             | GDK_KEY_ecircumflextilde
             | GDK_KEY_Ecircumflexbelowdot
             | GDK_KEY_ecircumflexbelowdot
             | GDK_KEY_Ihook
             | GDK_KEY_ihook
             | GDK_KEY_Ibelowdot
             | GDK_KEY_ibelowdot
             | GDK_KEY_Obelowdot
             | GDK_KEY_obelowdot
             | GDK_KEY_Ohook
             | GDK_KEY_ohook
             | GDK_KEY_Ocircumflexacute
             | GDK_KEY_ocircumflexacute
             | GDK_KEY_Ocircumflexgrave
             | GDK_KEY_ocircumflexgrave
             | GDK_KEY_Ocircumflexhook
             | GDK_KEY_ocircumflexhook
             | GDK_KEY_Ocircumflextilde
             | GDK_KEY_ocircumflextilde
             | GDK_KEY_Ocircumflexbelowdot
             | GDK_KEY_ocircumflexbelowdot
             | GDK_KEY_Ohornacute
             | GDK_KEY_ohornacute
             | GDK_KEY_Ohorngrave
             | GDK_KEY_ohorngrave
             | GDK_KEY_Ohornhook
             | GDK_KEY_ohornhook
             | GDK_KEY_Ohorntilde
             | GDK_KEY_ohorntilde
             | GDK_KEY_Ohornbelowdot
             | GDK_KEY_ohornbelowdot
             | GDK_KEY_Ubelowdot
             | GDK_KEY_ubelowdot
             | GDK_KEY_Uhook
             | GDK_KEY_uhook
             | GDK_KEY_Uhornacute
             | GDK_KEY_uhornacute
             | GDK_KEY_Uhorngrave
             | GDK_KEY_uhorngrave
             | GDK_KEY_Uhornhook
             | GDK_KEY_uhornhook
             | GDK_KEY_Uhorntilde
             | GDK_KEY_uhorntilde
             | GDK_KEY_Uhornbelowdot
             | GDK_KEY_uhornbelowdot
             | GDK_KEY_Ybelowdot
             | GDK_KEY_ybelowdot
             | GDK_KEY_Yhook
             | GDK_KEY_yhook
             | GDK_KEY_Ytilde
             | GDK_KEY_ytilde
             | GDK_KEY_Ohorn
             | GDK_KEY_ohorn
             | GDK_KEY_Uhorn
             | GDK_KEY_uhorn
             | GDK_KEY_EcuSign
             | GDK_KEY_ColonSign
             | GDK_KEY_CruzeiroSign
             | GDK_KEY_FFrancSign
             | GDK_KEY_LiraSign
             | GDK_KEY_MillSign
             | GDK_KEY_NairaSign
             | GDK_KEY_PesetaSign
             | GDK_KEY_RupeeSign
             | GDK_KEY_WonSign
             | GDK_KEY_NewSheqelSign
             | GDK_KEY_DongSign
             | GDK_KEY_EuroSign
             | GDK_KEY_zerosuperior
             | GDK_KEY_foursuperior
             | GDK_KEY_fivesuperior
             | GDK_KEY_sixsuperior
             | GDK_KEY_sevensuperior
             | GDK_KEY_eightsuperior
             | GDK_KEY_ninesuperior
             | GDK_KEY_zerosubscript
             | GDK_KEY_onesubscript
             | GDK_KEY_twosubscript
             | GDK_KEY_threesubscript
             | GDK_KEY_foursubscript
             | GDK_KEY_fivesubscript
             | GDK_KEY_sixsubscript
             | GDK_KEY_sevensubscript
             | GDK_KEY_eightsubscript
             | GDK_KEY_ninesubscript
             | GDK_KEY_partdifferential
             | GDK_KEY_emptyset
             | GDK_KEY_elementof
             | GDK_KEY_notelementof
             | GDK_KEY_containsas
             | GDK_KEY_squareroot
             | GDK_KEY_cuberoot
             | GDK_KEY_fourthroot
             | GDK_KEY_dintegral
             | GDK_KEY_tintegral
             | GDK_KEY_because
             | GDK_KEY_approxeq
             | GDK_KEY_notapproxeq
             | GDK_KEY_notidentical
             | GDK_KEY_stricteq
             | GDK_KEY_braille_dot_1
             | GDK_KEY_braille_dot_2
             | GDK_KEY_braille_dot_3
             | GDK_KEY_braille_dot_4
             | GDK_KEY_braille_dot_5
             | GDK_KEY_braille_dot_6
             | GDK_KEY_braille_dot_7
             | GDK_KEY_braille_dot_8
             | GDK_KEY_braille_dot_9
             | GDK_KEY_braille_dot_10
             | GDK_KEY_braille_blank
             | GDK_KEY_braille_dots_1
             | GDK_KEY_braille_dots_2
             | GDK_KEY_braille_dots_12
             | GDK_KEY_braille_dots_3
             | GDK_KEY_braille_dots_13
             | GDK_KEY_braille_dots_23
             | GDK_KEY_braille_dots_123
             | GDK_KEY_braille_dots_4
             | GDK_KEY_braille_dots_14
             | GDK_KEY_braille_dots_24
             | GDK_KEY_braille_dots_124
             | GDK_KEY_braille_dots_34
             | GDK_KEY_braille_dots_134
             | GDK_KEY_braille_dots_234
             | GDK_KEY_braille_dots_1234
             | GDK_KEY_braille_dots_5
             | GDK_KEY_braille_dots_15
             | GDK_KEY_braille_dots_25
             | GDK_KEY_braille_dots_125
             | GDK_KEY_braille_dots_35
             | GDK_KEY_braille_dots_135
             | GDK_KEY_braille_dots_235
             | GDK_KEY_braille_dots_1235
             | GDK_KEY_braille_dots_45
             | GDK_KEY_braille_dots_145
             | GDK_KEY_braille_dots_245
             | GDK_KEY_braille_dots_1245
             | GDK_KEY_braille_dots_345
             | GDK_KEY_braille_dots_1345
             | GDK_KEY_braille_dots_2345
             | GDK_KEY_braille_dots_12345
             | GDK_KEY_braille_dots_6
             | GDK_KEY_braille_dots_16
             | GDK_KEY_braille_dots_26
             | GDK_KEY_braille_dots_126
             | GDK_KEY_braille_dots_36
             | GDK_KEY_braille_dots_136
             | GDK_KEY_braille_dots_236
             | GDK_KEY_braille_dots_1236
             | GDK_KEY_braille_dots_46
             | GDK_KEY_braille_dots_146
             | GDK_KEY_braille_dots_246
             | GDK_KEY_braille_dots_1246
             | GDK_KEY_braille_dots_346
             | GDK_KEY_braille_dots_1346
             | GDK_KEY_braille_dots_2346
             | GDK_KEY_braille_dots_12346
             | GDK_KEY_braille_dots_56
             | GDK_KEY_braille_dots_156
             | GDK_KEY_braille_dots_256
             | GDK_KEY_braille_dots_1256
             | GDK_KEY_braille_dots_356
             | GDK_KEY_braille_dots_1356
             | GDK_KEY_braille_dots_2356
             | GDK_KEY_braille_dots_12356
             | GDK_KEY_braille_dots_456
             | GDK_KEY_braille_dots_1456
             | GDK_KEY_braille_dots_2456
             | GDK_KEY_braille_dots_12456
             | GDK_KEY_braille_dots_3456
             | GDK_KEY_braille_dots_13456
             | GDK_KEY_braille_dots_23456
             | GDK_KEY_braille_dots_123456
             | GDK_KEY_braille_dots_7
             | GDK_KEY_braille_dots_17
             | GDK_KEY_braille_dots_27
             | GDK_KEY_braille_dots_127
             | GDK_KEY_braille_dots_37
             | GDK_KEY_braille_dots_137
             | GDK_KEY_braille_dots_237
             | GDK_KEY_braille_dots_1237
             | GDK_KEY_braille_dots_47
             | GDK_KEY_braille_dots_147
             | GDK_KEY_braille_dots_247
             | GDK_KEY_braille_dots_1247
             | GDK_KEY_braille_dots_347
             | GDK_KEY_braille_dots_1347
             | GDK_KEY_braille_dots_2347
             | GDK_KEY_braille_dots_12347
             | GDK_KEY_braille_dots_57
             | GDK_KEY_braille_dots_157
             | GDK_KEY_braille_dots_257
             | GDK_KEY_braille_dots_1257
             | GDK_KEY_braille_dots_357
             | GDK_KEY_braille_dots_1357
             | GDK_KEY_braille_dots_2357
             | GDK_KEY_braille_dots_12357
             | GDK_KEY_braille_dots_457
             | GDK_KEY_braille_dots_1457
             | GDK_KEY_braille_dots_2457
             | GDK_KEY_braille_dots_12457
             | GDK_KEY_braille_dots_3457
             | GDK_KEY_braille_dots_13457
             | GDK_KEY_braille_dots_23457
             | GDK_KEY_braille_dots_123457
             | GDK_KEY_braille_dots_67
             | GDK_KEY_braille_dots_167
             | GDK_KEY_braille_dots_267
             | GDK_KEY_braille_dots_1267
             | GDK_KEY_braille_dots_367
             | GDK_KEY_braille_dots_1367
             | GDK_KEY_braille_dots_2367
             | GDK_KEY_braille_dots_12367
             | GDK_KEY_braille_dots_467
             | GDK_KEY_braille_dots_1467
             | GDK_KEY_braille_dots_2467
             | GDK_KEY_braille_dots_12467
             | GDK_KEY_braille_dots_3467
             | GDK_KEY_braille_dots_13467
             | GDK_KEY_braille_dots_23467
             | GDK_KEY_braille_dots_123467
             | GDK_KEY_braille_dots_567
             | GDK_KEY_braille_dots_1567
             | GDK_KEY_braille_dots_2567
             | GDK_KEY_braille_dots_12567
             | GDK_KEY_braille_dots_3567
             | GDK_KEY_braille_dots_13567
             | GDK_KEY_braille_dots_23567
             | GDK_KEY_braille_dots_123567
             | GDK_KEY_braille_dots_4567
             | GDK_KEY_braille_dots_14567
             | GDK_KEY_braille_dots_24567
             | GDK_KEY_braille_dots_124567
             | GDK_KEY_braille_dots_34567
             | GDK_KEY_braille_dots_134567
             | GDK_KEY_braille_dots_234567
             | GDK_KEY_braille_dots_1234567
             | GDK_KEY_braille_dots_8
             | GDK_KEY_braille_dots_18
             | GDK_KEY_braille_dots_28
             | GDK_KEY_braille_dots_128
             | GDK_KEY_braille_dots_38
             | GDK_KEY_braille_dots_138
             | GDK_KEY_braille_dots_238
             | GDK_KEY_braille_dots_1238
             | GDK_KEY_braille_dots_48
             | GDK_KEY_braille_dots_148
             | GDK_KEY_braille_dots_248
             | GDK_KEY_braille_dots_1248
             | GDK_KEY_braille_dots_348
             | GDK_KEY_braille_dots_1348
             | GDK_KEY_braille_dots_2348
             | GDK_KEY_braille_dots_12348
             | GDK_KEY_braille_dots_58
             | GDK_KEY_braille_dots_158
             | GDK_KEY_braille_dots_258
             | GDK_KEY_braille_dots_1258
             | GDK_KEY_braille_dots_358
             | GDK_KEY_braille_dots_1358
             | GDK_KEY_braille_dots_2358
             | GDK_KEY_braille_dots_12358
             | GDK_KEY_braille_dots_458
             | GDK_KEY_braille_dots_1458
             | GDK_KEY_braille_dots_2458
             | GDK_KEY_braille_dots_12458
             | GDK_KEY_braille_dots_3458
             | GDK_KEY_braille_dots_13458
             | GDK_KEY_braille_dots_23458
             | GDK_KEY_braille_dots_123458
             | GDK_KEY_braille_dots_68
             | GDK_KEY_braille_dots_168
             | GDK_KEY_braille_dots_268
             | GDK_KEY_braille_dots_1268
             | GDK_KEY_braille_dots_368
             | GDK_KEY_braille_dots_1368
             | GDK_KEY_braille_dots_2368
             | GDK_KEY_braille_dots_12368
             | GDK_KEY_braille_dots_468
             | GDK_KEY_braille_dots_1468
             | GDK_KEY_braille_dots_2468
             | GDK_KEY_braille_dots_12468
             | GDK_KEY_braille_dots_3468
             | GDK_KEY_braille_dots_13468
             | GDK_KEY_braille_dots_23468
             | GDK_KEY_braille_dots_123468
             | GDK_KEY_braille_dots_568
             | GDK_KEY_braille_dots_1568
             | GDK_KEY_braille_dots_2568
             | GDK_KEY_braille_dots_12568
             | GDK_KEY_braille_dots_3568
             | GDK_KEY_braille_dots_13568
             | GDK_KEY_braille_dots_23568
             | GDK_KEY_braille_dots_123568
             | GDK_KEY_braille_dots_4568
             | GDK_KEY_braille_dots_14568
             | GDK_KEY_braille_dots_24568
             | GDK_KEY_braille_dots_124568
             | GDK_KEY_braille_dots_34568
             | GDK_KEY_braille_dots_134568
             | GDK_KEY_braille_dots_234568
             | GDK_KEY_braille_dots_1234568
             | GDK_KEY_braille_dots_78
             | GDK_KEY_braille_dots_178
             | GDK_KEY_braille_dots_278
             | GDK_KEY_braille_dots_1278
             | GDK_KEY_braille_dots_378
             | GDK_KEY_braille_dots_1378
             | GDK_KEY_braille_dots_2378
             | GDK_KEY_braille_dots_12378
             | GDK_KEY_braille_dots_478
             | GDK_KEY_braille_dots_1478
             | GDK_KEY_braille_dots_2478
             | GDK_KEY_braille_dots_12478
             | GDK_KEY_braille_dots_3478
             | GDK_KEY_braille_dots_13478
             | GDK_KEY_braille_dots_23478
             | GDK_KEY_braille_dots_123478
             | GDK_KEY_braille_dots_578
             | GDK_KEY_braille_dots_1578
             | GDK_KEY_braille_dots_2578
             | GDK_KEY_braille_dots_12578
             | GDK_KEY_braille_dots_3578
             | GDK_KEY_braille_dots_13578
             | GDK_KEY_braille_dots_23578
             | GDK_KEY_braille_dots_123578
             | GDK_KEY_braille_dots_4578
             | GDK_KEY_braille_dots_14578
             | GDK_KEY_braille_dots_24578
             | GDK_KEY_braille_dots_124578
             | GDK_KEY_braille_dots_34578
             | GDK_KEY_braille_dots_134578
             | GDK_KEY_braille_dots_234578
             | GDK_KEY_braille_dots_1234578
             | GDK_KEY_braille_dots_678
             | GDK_KEY_braille_dots_1678
             | GDK_KEY_braille_dots_2678
             | GDK_KEY_braille_dots_12678
             | GDK_KEY_braille_dots_3678
             | GDK_KEY_braille_dots_13678
             | GDK_KEY_braille_dots_23678
             | GDK_KEY_braille_dots_123678
             | GDK_KEY_braille_dots_4678
             | GDK_KEY_braille_dots_14678
             | GDK_KEY_braille_dots_24678
             | GDK_KEY_braille_dots_124678
             | GDK_KEY_braille_dots_34678
             | GDK_KEY_braille_dots_134678
             | GDK_KEY_braille_dots_234678
             | GDK_KEY_braille_dots_1234678
             | GDK_KEY_braille_dots_5678
             | GDK_KEY_braille_dots_15678
             | GDK_KEY_braille_dots_25678
             | GDK_KEY_braille_dots_125678
             | GDK_KEY_braille_dots_35678
             | GDK_KEY_braille_dots_135678
             | GDK_KEY_braille_dots_235678
             | GDK_KEY_braille_dots_1235678
             | GDK_KEY_braille_dots_45678
             | GDK_KEY_braille_dots_145678
             | GDK_KEY_braille_dots_245678
             | GDK_KEY_braille_dots_1245678
             | GDK_KEY_braille_dots_345678
             | GDK_KEY_braille_dots_1345678
             | GDK_KEY_braille_dots_2345678
             | GDK_KEY_braille_dots_12345678
             | GDK_KEY_Sinh_ng
             | GDK_KEY_Sinh_h2
             | GDK_KEY_Sinh_a
             | GDK_KEY_Sinh_aa
             | GDK_KEY_Sinh_ae
             | GDK_KEY_Sinh_aee
             | GDK_KEY_Sinh_i
             | GDK_KEY_Sinh_ii
             | GDK_KEY_Sinh_u
             | GDK_KEY_Sinh_uu
             | GDK_KEY_Sinh_ri
             | GDK_KEY_Sinh_rii
             | GDK_KEY_Sinh_lu
             | GDK_KEY_Sinh_luu
             | GDK_KEY_Sinh_e
             | GDK_KEY_Sinh_ee
             | GDK_KEY_Sinh_ai
             | GDK_KEY_Sinh_o
             | GDK_KEY_Sinh_oo
             | GDK_KEY_Sinh_au
             | GDK_KEY_Sinh_ka
             | GDK_KEY_Sinh_kha
             | GDK_KEY_Sinh_ga
             | GDK_KEY_Sinh_gha
             | GDK_KEY_Sinh_ng2
             | GDK_KEY_Sinh_nga
             | GDK_KEY_Sinh_ca
             | GDK_KEY_Sinh_cha
             | GDK_KEY_Sinh_ja
             | GDK_KEY_Sinh_jha
             | GDK_KEY_Sinh_nya
             | GDK_KEY_Sinh_jnya
             | GDK_KEY_Sinh_nja
             | GDK_KEY_Sinh_tta
             | GDK_KEY_Sinh_ttha
             | GDK_KEY_Sinh_dda
             | GDK_KEY_Sinh_ddha
             | GDK_KEY_Sinh_nna
             | GDK_KEY_Sinh_ndda
             | GDK_KEY_Sinh_tha
             | GDK_KEY_Sinh_thha
             | GDK_KEY_Sinh_dha
             | GDK_KEY_Sinh_dhha
             | GDK_KEY_Sinh_na
             | GDK_KEY_Sinh_ndha
             | GDK_KEY_Sinh_pa
             | GDK_KEY_Sinh_pha
             | GDK_KEY_Sinh_ba
             | GDK_KEY_Sinh_bha
             | GDK_KEY_Sinh_ma
             | GDK_KEY_Sinh_mba
             | GDK_KEY_Sinh_ya
             | GDK_KEY_Sinh_ra
             | GDK_KEY_Sinh_la
             | GDK_KEY_Sinh_va
             | GDK_KEY_Sinh_sha
             | GDK_KEY_Sinh_ssha
             | GDK_KEY_Sinh_sa
             | GDK_KEY_Sinh_ha
             | GDK_KEY_Sinh_lla
             | GDK_KEY_Sinh_fa
             | GDK_KEY_Sinh_al
             | GDK_KEY_Sinh_aa2
             | GDK_KEY_Sinh_ae2
             | GDK_KEY_Sinh_aee2
             | GDK_KEY_Sinh_i2
             | GDK_KEY_Sinh_ii2
             | GDK_KEY_Sinh_u2
             | GDK_KEY_Sinh_uu2
             | GDK_KEY_Sinh_ru2
             | GDK_KEY_Sinh_e2
             | GDK_KEY_Sinh_ee2
             | GDK_KEY_Sinh_ai2
             | GDK_KEY_Sinh_o2
             | GDK_KEY_Sinh_oo2
             | GDK_KEY_Sinh_au2
             | GDK_KEY_Sinh_lu2
             | GDK_KEY_Sinh_ruu2
             | GDK_KEY_Sinh_luu2
             | GDK_KEY_Sinh_kunddaliya
             | GDK_KEY_ModeLock
             | GDK_KEY_MonBrightnessUp
             | GDK_KEY_MonBrightnessDown
             | GDK_KEY_KbdLightOnOff
             | GDK_KEY_KbdBrightnessUp
             | GDK_KEY_KbdBrightnessDown
             | GDK_KEY_Standby
             | GDK_KEY_AudioLowerVolume
             | GDK_KEY_AudioMute
             | GDK_KEY_AudioRaiseVolume
             | GDK_KEY_AudioPlay
             | GDK_KEY_AudioStop
             | GDK_KEY_AudioPrev
             | GDK_KEY_AudioNext
             | GDK_KEY_HomePage
             | GDK_KEY_Mail
             | GDK_KEY_Start
             | GDK_KEY_Search
             | GDK_KEY_AudioRecord
             | GDK_KEY_Calculator
             | GDK_KEY_Memo
             | GDK_KEY_ToDoList
             | GDK_KEY_Calendar
             | GDK_KEY_PowerDown
             | GDK_KEY_ContrastAdjust
             | GDK_KEY_RockerUp
             | GDK_KEY_RockerDown
             | GDK_KEY_RockerEnter
             | GDK_KEY_Back
             | GDK_KEY_Forward
             | GDK_KEY_Stop
             | GDK_KEY_Refresh
             | GDK_KEY_PowerOff
             | GDK_KEY_WakeUp
             | GDK_KEY_Eject
             | GDK_KEY_ScreenSaver
             | GDK_KEY_WWW
             | GDK_KEY_Sleep
             | GDK_KEY_Favorites
             | GDK_KEY_AudioPause
             | GDK_KEY_AudioMedia
             | GDK_KEY_MyComputer
             | GDK_KEY_VendorHome
             | GDK_KEY_LightBulb
             | GDK_KEY_Shop
             | GDK_KEY_History
             | GDK_KEY_OpenURL
             | GDK_KEY_AddFavorite
             | GDK_KEY_HotLinks
             | GDK_KEY_BrightnessAdjust
             | GDK_KEY_Finance
             | GDK_KEY_Community
             | GDK_KEY_AudioRewind
             | GDK_KEY_BackForward
             | GDK_KEY_Launch0
             | GDK_KEY_Launch1
             | GDK_KEY_Launch2
             | GDK_KEY_Launch3
             | GDK_KEY_Launch4
             | GDK_KEY_Launch5
             | GDK_KEY_Launch6
             | GDK_KEY_Launch7
             | GDK_KEY_Launch8
             | GDK_KEY_Launch9
             | GDK_KEY_LaunchA
             | GDK_KEY_LaunchB
             | GDK_KEY_LaunchC
             | GDK_KEY_LaunchD
             | GDK_KEY_LaunchE
             | GDK_KEY_LaunchF
             | GDK_KEY_ApplicationLeft
             | GDK_KEY_ApplicationRight
             | GDK_KEY_Book
             | GDK_KEY_CD
             | GDK_KEY_WindowClear
             | GDK_KEY_Close
             | GDK_KEY_Copy
             | GDK_KEY_Cut
             | GDK_KEY_Display
             | GDK_KEY_DOS
             | GDK_KEY_Documents
             | GDK_KEY_Excel
             | GDK_KEY_Explorer
             | GDK_KEY_Game
             | GDK_KEY_Go
             | GDK_KEY_iTouch
             | GDK_KEY_LogOff
             | GDK_KEY_Market
             | GDK_KEY_Meeting
             | GDK_KEY_MenuKB
             | GDK_KEY_MenuPB
             | GDK_KEY_MySites
             | GDK_KEY_New
             | GDK_KEY_News
             | GDK_KEY_OfficeHome
             | GDK_KEY_Open
             | GDK_KEY_Option
             | GDK_KEY_Paste
             | GDK_KEY_Phone
             | GDK_KEY_Reply
             | GDK_KEY_Reload
             | GDK_KEY_RotateWindows
             | GDK_KEY_RotationPB
             | GDK_KEY_RotationKB
             | GDK_KEY_Save
             | GDK_KEY_ScrollUp
             | GDK_KEY_ScrollDown
             | GDK_KEY_ScrollClick
             | GDK_KEY_Send
             | GDK_KEY_Spell
             | GDK_KEY_SplitScreen
             | GDK_KEY_Support
             | GDK_KEY_TaskPane
             | GDK_KEY_Terminal
             | GDK_KEY_Tools
             | GDK_KEY_Travel
             | GDK_KEY_UserPB
             | GDK_KEY_User1KB
             | GDK_KEY_User2KB
             | GDK_KEY_Video
             | GDK_KEY_WheelButton
             | GDK_KEY_Word
             | GDK_KEY_Xfer
             | GDK_KEY_ZoomIn
             | GDK_KEY_ZoomOut
             | GDK_KEY_Away
             | GDK_KEY_Messenger
             | GDK_KEY_WebCam
             | GDK_KEY_MailForward
             | GDK_KEY_Pictures
             | GDK_KEY_Music
             | GDK_KEY_Battery
             | GDK_KEY_Bluetooth
             | GDK_KEY_WLAN
             | GDK_KEY_UWB
             | GDK_KEY_AudioForward
             | GDK_KEY_AudioRepeat
             | GDK_KEY_AudioRandomPlay
             | GDK_KEY_Subtitle
             | GDK_KEY_AudioCycleTrack
             | GDK_KEY_CycleAngle
             | GDK_KEY_FrameBack
             | GDK_KEY_FrameForward
             | GDK_KEY_Time
             | GDK_KEY_SelectButton
             | GDK_KEY_View
             | GDK_KEY_TopMenu
             | GDK_KEY_Red
             | GDK_KEY_Green
             | GDK_KEY_Yellow
             | GDK_KEY_Blue
             | GDK_KEY_Suspend
             | GDK_KEY_Hibernate
             | GDK_KEY_TouchpadToggle
             | GDK_KEY_TouchpadOn
             | GDK_KEY_TouchpadOff
             | GDK_KEY_AudioMicMute
             | GDK_KEY_Switch_VT_1
             | GDK_KEY_Switch_VT_2
             | GDK_KEY_Switch_VT_3
             | GDK_KEY_Switch_VT_4
             | GDK_KEY_Switch_VT_5
             | GDK_KEY_Switch_VT_6
             | GDK_KEY_Switch_VT_7
             | GDK_KEY_Switch_VT_8
             | GDK_KEY_Switch_VT_9
             | GDK_KEY_Switch_VT_10
             | GDK_KEY_Switch_VT_11
             | GDK_KEY_Switch_VT_12
             | GDK_KEY_Ungrab
             | GDK_KEY_ClearGrab
             | GDK_KEY_Next_VMode
             | GDK_KEY_Prev_VMode
             | GDK_KEY_LogWindowTree
             | GDK_KEY_LogGrabInfo
             deriving Show

instance Eq KeyCode where a == b = fromEnum a == fromEnum b
instance Ord KeyCode where compare = comparing fromEnum

instance Enum KeyCode where
  fromEnum GDK_KEY_VoidSymbol = 0xffffff
  fromEnum GDK_KEY_BackSpace = 0xff08
  fromEnum GDK_KEY_Tab = 0xff09
  fromEnum GDK_KEY_Linefeed = 0xff0a
  fromEnum GDK_KEY_Clear = 0xff0b
  fromEnum GDK_KEY_Return = 0xff0d
  fromEnum GDK_KEY_Pause = 0xff13
  fromEnum GDK_KEY_Scroll_Lock = 0xff14
  fromEnum GDK_KEY_Sys_Req = 0xff15
  fromEnum GDK_KEY_Escape = 0xff1b
  fromEnum GDK_KEY_Delete = 0xffff
  fromEnum GDK_KEY_Multi_key = 0xff20
  fromEnum GDK_KEY_Codeinput = 0xff37
  fromEnum GDK_KEY_SingleCandidate = 0xff3c
  fromEnum GDK_KEY_MultipleCandidate = 0xff3d
  fromEnum GDK_KEY_PreviousCandidate = 0xff3e
  fromEnum GDK_KEY_Kanji = 0xff21
  fromEnum GDK_KEY_Muhenkan = 0xff22
  fromEnum GDK_KEY_Henkan_Mode = 0xff23
  fromEnum GDK_KEY_Henkan = 0xff23
  fromEnum GDK_KEY_Romaji = 0xff24
  fromEnum GDK_KEY_Hiragana = 0xff25
  fromEnum GDK_KEY_Katakana = 0xff26
  fromEnum GDK_KEY_Hiragana_Katakana = 0xff27
  fromEnum GDK_KEY_Zenkaku = 0xff28
  fromEnum GDK_KEY_Hankaku = 0xff29
  fromEnum GDK_KEY_Zenkaku_Hankaku = 0xff2a
  fromEnum GDK_KEY_Touroku = 0xff2b
  fromEnum GDK_KEY_Massyo = 0xff2c
  fromEnum GDK_KEY_Kana_Lock = 0xff2d
  fromEnum GDK_KEY_Kana_Shift = 0xff2e
  fromEnum GDK_KEY_Eisu_Shift = 0xff2f
  fromEnum GDK_KEY_Eisu_toggle = 0xff30
  fromEnum GDK_KEY_Kanji_Bangou = 0xff37
  fromEnum GDK_KEY_Zen_Koho = 0xff3d
  fromEnum GDK_KEY_Mae_Koho = 0xff3e
  fromEnum GDK_KEY_Home = 0xff50
  fromEnum GDK_KEY_Left = 0xff51
  fromEnum GDK_KEY_Up = 0xff52
  fromEnum GDK_KEY_Right = 0xff53
  fromEnum GDK_KEY_Down = 0xff54
  fromEnum GDK_KEY_Prior = 0xff55
  fromEnum GDK_KEY_Page_Up = 0xff55
  fromEnum GDK_KEY_Next = 0xff56
  fromEnum GDK_KEY_Page_Down = 0xff56
  fromEnum GDK_KEY_End = 0xff57
  fromEnum GDK_KEY_Begin = 0xff58
  fromEnum GDK_KEY_Select = 0xff60
  fromEnum GDK_KEY_Print = 0xff61
  fromEnum GDK_KEY_Execute = 0xff62
  fromEnum GDK_KEY_Insert = 0xff63
  fromEnum GDK_KEY_Undo = 0xff65
  fromEnum GDK_KEY_Redo = 0xff66
  fromEnum GDK_KEY_Menu = 0xff67
  fromEnum GDK_KEY_Find = 0xff68
  fromEnum GDK_KEY_Cancel = 0xff69
  fromEnum GDK_KEY_Help = 0xff6a
  fromEnum GDK_KEY_Break = 0xff6b
  fromEnum GDK_KEY_Mode_switch = 0xff7e
  fromEnum GDK_KEY_script_switch = 0xff7e
  fromEnum GDK_KEY_Num_Lock = 0xff7f
  fromEnum GDK_KEY_KP_Space = 0xff80
  fromEnum GDK_KEY_KP_Tab = 0xff89
  fromEnum GDK_KEY_KP_Enter = 0xff8d
  fromEnum GDK_KEY_KP_F1 = 0xff91
  fromEnum GDK_KEY_KP_F2 = 0xff92
  fromEnum GDK_KEY_KP_F3 = 0xff93
  fromEnum GDK_KEY_KP_F4 = 0xff94
  fromEnum GDK_KEY_KP_Home = 0xff95
  fromEnum GDK_KEY_KP_Left = 0xff96
  fromEnum GDK_KEY_KP_Up = 0xff97
  fromEnum GDK_KEY_KP_Right = 0xff98
  fromEnum GDK_KEY_KP_Down = 0xff99
  fromEnum GDK_KEY_KP_Prior = 0xff9a
  fromEnum GDK_KEY_KP_Page_Up = 0xff9a
  fromEnum GDK_KEY_KP_Next = 0xff9b
  fromEnum GDK_KEY_KP_Page_Down = 0xff9b
  fromEnum GDK_KEY_KP_End = 0xff9c
  fromEnum GDK_KEY_KP_Begin = 0xff9d
  fromEnum GDK_KEY_KP_Insert = 0xff9e
  fromEnum GDK_KEY_KP_Delete = 0xff9f
  fromEnum GDK_KEY_KP_Equal = 0xffbd
  fromEnum GDK_KEY_KP_Multiply = 0xffaa
  fromEnum GDK_KEY_KP_Add = 0xffab
  fromEnum GDK_KEY_KP_Separator = 0xffac
  fromEnum GDK_KEY_KP_Subtract = 0xffad
  fromEnum GDK_KEY_KP_Decimal = 0xffae
  fromEnum GDK_KEY_KP_Divide = 0xffaf
  fromEnum GDK_KEY_KP_0 = 0xffb0
  fromEnum GDK_KEY_KP_1 = 0xffb1
  fromEnum GDK_KEY_KP_2 = 0xffb2
  fromEnum GDK_KEY_KP_3 = 0xffb3
  fromEnum GDK_KEY_KP_4 = 0xffb4
  fromEnum GDK_KEY_KP_5 = 0xffb5
  fromEnum GDK_KEY_KP_6 = 0xffb6
  fromEnum GDK_KEY_KP_7 = 0xffb7
  fromEnum GDK_KEY_KP_8 = 0xffb8
  fromEnum GDK_KEY_KP_9 = 0xffb9
  fromEnum GDK_KEY_F1 = 0xffbe
  fromEnum GDK_KEY_F2 = 0xffbf
  fromEnum GDK_KEY_F3 = 0xffc0
  fromEnum GDK_KEY_F4 = 0xffc1
  fromEnum GDK_KEY_F5 = 0xffc2
  fromEnum GDK_KEY_F6 = 0xffc3
  fromEnum GDK_KEY_F7 = 0xffc4
  fromEnum GDK_KEY_F8 = 0xffc5
  fromEnum GDK_KEY_F9 = 0xffc6
  fromEnum GDK_KEY_F10 = 0xffc7
  fromEnum GDK_KEY_F11 = 0xffc8
  fromEnum GDK_KEY_L1 = 0xffc8
  fromEnum GDK_KEY_F12 = 0xffc9
  fromEnum GDK_KEY_L2 = 0xffc9
  fromEnum GDK_KEY_F13 = 0xffca
  fromEnum GDK_KEY_L3 = 0xffca
  fromEnum GDK_KEY_F14 = 0xffcb
  fromEnum GDK_KEY_L4 = 0xffcb
  fromEnum GDK_KEY_F15 = 0xffcc
  fromEnum GDK_KEY_L5 = 0xffcc
  fromEnum GDK_KEY_F16 = 0xffcd
  fromEnum GDK_KEY_L6 = 0xffcd
  fromEnum GDK_KEY_F17 = 0xffce
  fromEnum GDK_KEY_L7 = 0xffce
  fromEnum GDK_KEY_F18 = 0xffcf
  fromEnum GDK_KEY_L8 = 0xffcf
  fromEnum GDK_KEY_F19 = 0xffd0
  fromEnum GDK_KEY_L9 = 0xffd0
  fromEnum GDK_KEY_F20 = 0xffd1
  fromEnum GDK_KEY_L10 = 0xffd1
  fromEnum GDK_KEY_F21 = 0xffd2
  fromEnum GDK_KEY_R1 = 0xffd2
  fromEnum GDK_KEY_F22 = 0xffd3
  fromEnum GDK_KEY_R2 = 0xffd3
  fromEnum GDK_KEY_F23 = 0xffd4
  fromEnum GDK_KEY_R3 = 0xffd4
  fromEnum GDK_KEY_F24 = 0xffd5
  fromEnum GDK_KEY_R4 = 0xffd5
  fromEnum GDK_KEY_F25 = 0xffd6
  fromEnum GDK_KEY_R5 = 0xffd6
  fromEnum GDK_KEY_F26 = 0xffd7
  fromEnum GDK_KEY_R6 = 0xffd7
  fromEnum GDK_KEY_F27 = 0xffd8
  fromEnum GDK_KEY_R7 = 0xffd8
  fromEnum GDK_KEY_F28 = 0xffd9
  fromEnum GDK_KEY_R8 = 0xffd9
  fromEnum GDK_KEY_F29 = 0xffda
  fromEnum GDK_KEY_R9 = 0xffda
  fromEnum GDK_KEY_F30 = 0xffdb
  fromEnum GDK_KEY_R10 = 0xffdb
  fromEnum GDK_KEY_F31 = 0xffdc
  fromEnum GDK_KEY_R11 = 0xffdc
  fromEnum GDK_KEY_F32 = 0xffdd
  fromEnum GDK_KEY_R12 = 0xffdd
  fromEnum GDK_KEY_F33 = 0xffde
  fromEnum GDK_KEY_R13 = 0xffde
  fromEnum GDK_KEY_F34 = 0xffdf
  fromEnum GDK_KEY_R14 = 0xffdf
  fromEnum GDK_KEY_F35 = 0xffe0
  fromEnum GDK_KEY_R15 = 0xffe0
  fromEnum GDK_KEY_Shift_L = 0xffe1
  fromEnum GDK_KEY_Shift_R = 0xffe2
  fromEnum GDK_KEY_Control_L = 0xffe3
  fromEnum GDK_KEY_Control_R = 0xffe4
  fromEnum GDK_KEY_Caps_Lock = 0xffe5
  fromEnum GDK_KEY_Shift_Lock = 0xffe6
  fromEnum GDK_KEY_Meta_L = 0xffe7
  fromEnum GDK_KEY_Meta_R = 0xffe8
  fromEnum GDK_KEY_Alt_L = 0xffe9
  fromEnum GDK_KEY_Alt_R = 0xffea
  fromEnum GDK_KEY_Super_L = 0xffeb
  fromEnum GDK_KEY_Super_R = 0xffec
  fromEnum GDK_KEY_Hyper_L = 0xffed
  fromEnum GDK_KEY_Hyper_R = 0xffee
  fromEnum GDK_KEY_ISO_Lock = 0xfe01
  fromEnum GDK_KEY_ISO_Level2_Latch = 0xfe02
  fromEnum GDK_KEY_ISO_Level3_Shift = 0xfe03
  fromEnum GDK_KEY_ISO_Level3_Latch = 0xfe04
  fromEnum GDK_KEY_ISO_Level3_Lock = 0xfe05
  fromEnum GDK_KEY_ISO_Level5_Shift = 0xfe11
  fromEnum GDK_KEY_ISO_Level5_Latch = 0xfe12
  fromEnum GDK_KEY_ISO_Level5_Lock = 0xfe13
  fromEnum GDK_KEY_ISO_Group_Shift = 0xff7e
  fromEnum GDK_KEY_ISO_Group_Latch = 0xfe06
  fromEnum GDK_KEY_ISO_Group_Lock = 0xfe07
  fromEnum GDK_KEY_ISO_Next_Group = 0xfe08
  fromEnum GDK_KEY_ISO_Next_Group_Lock = 0xfe09
  fromEnum GDK_KEY_ISO_Prev_Group = 0xfe0a
  fromEnum GDK_KEY_ISO_Prev_Group_Lock = 0xfe0b
  fromEnum GDK_KEY_ISO_First_Group = 0xfe0c
  fromEnum GDK_KEY_ISO_First_Group_Lock = 0xfe0d
  fromEnum GDK_KEY_ISO_Last_Group = 0xfe0e
  fromEnum GDK_KEY_ISO_Last_Group_Lock = 0xfe0f
  fromEnum GDK_KEY_ISO_Left_Tab = 0xfe20
  fromEnum GDK_KEY_ISO_Move_Line_Up = 0xfe21
  fromEnum GDK_KEY_ISO_Move_Line_Down = 0xfe22
  fromEnum GDK_KEY_ISO_Partial_Line_Up = 0xfe23
  fromEnum GDK_KEY_ISO_Partial_Line_Down = 0xfe24
  fromEnum GDK_KEY_ISO_Partial_Space_Left = 0xfe25
  fromEnum GDK_KEY_ISO_Partial_Space_Right = 0xfe26
  fromEnum GDK_KEY_ISO_Set_Margin_Left = 0xfe27
  fromEnum GDK_KEY_ISO_Set_Margin_Right = 0xfe28
  fromEnum GDK_KEY_ISO_Release_Margin_Left = 0xfe29
  fromEnum GDK_KEY_ISO_Release_Margin_Right = 0xfe2a
  fromEnum GDK_KEY_ISO_Release_Both_Margins = 0xfe2b
  fromEnum GDK_KEY_ISO_Fast_Cursor_Left = 0xfe2c
  fromEnum GDK_KEY_ISO_Fast_Cursor_Right = 0xfe2d
  fromEnum GDK_KEY_ISO_Fast_Cursor_Up = 0xfe2e
  fromEnum GDK_KEY_ISO_Fast_Cursor_Down = 0xfe2f
  fromEnum GDK_KEY_ISO_Continuous_Underline = 0xfe30
  fromEnum GDK_KEY_ISO_Discontinuous_Underline = 0xfe31
  fromEnum GDK_KEY_ISO_Emphasize = 0xfe32
  fromEnum GDK_KEY_ISO_Center_Object = 0xfe33
  fromEnum GDK_KEY_ISO_Enter = 0xfe34
  fromEnum GDK_KEY_dead_grave = 0xfe50
  fromEnum GDK_KEY_dead_acute = 0xfe51
  fromEnum GDK_KEY_dead_circumflex = 0xfe52
  fromEnum GDK_KEY_dead_tilde = 0xfe53
  fromEnum GDK_KEY_dead_perispomeni = 0xfe53
  fromEnum GDK_KEY_dead_macron = 0xfe54
  fromEnum GDK_KEY_dead_breve = 0xfe55
  fromEnum GDK_KEY_dead_abovedot = 0xfe56
  fromEnum GDK_KEY_dead_diaeresis = 0xfe57
  fromEnum GDK_KEY_dead_abovering = 0xfe58
  fromEnum GDK_KEY_dead_doubleacute = 0xfe59
  fromEnum GDK_KEY_dead_caron = 0xfe5a
  fromEnum GDK_KEY_dead_cedilla = 0xfe5b
  fromEnum GDK_KEY_dead_ogonek = 0xfe5c
  fromEnum GDK_KEY_dead_iota = 0xfe5d
  fromEnum GDK_KEY_dead_voiced_sound = 0xfe5e
  fromEnum GDK_KEY_dead_semivoiced_sound = 0xfe5f
  fromEnum GDK_KEY_dead_belowdot = 0xfe60
  fromEnum GDK_KEY_dead_hook = 0xfe61
  fromEnum GDK_KEY_dead_horn = 0xfe62
  fromEnum GDK_KEY_dead_stroke = 0xfe63
  fromEnum GDK_KEY_dead_abovecomma = 0xfe64
  fromEnum GDK_KEY_dead_psili = 0xfe64
  fromEnum GDK_KEY_dead_abovereversedcomma = 0xfe65
  fromEnum GDK_KEY_dead_dasia = 0xfe65
  fromEnum GDK_KEY_dead_doublegrave = 0xfe66
  fromEnum GDK_KEY_dead_belowring = 0xfe67
  fromEnum GDK_KEY_dead_belowmacron = 0xfe68
  fromEnum GDK_KEY_dead_belowcircumflex = 0xfe69
  fromEnum GDK_KEY_dead_belowtilde = 0xfe6a
  fromEnum GDK_KEY_dead_belowbreve = 0xfe6b
  fromEnum GDK_KEY_dead_belowdiaeresis = 0xfe6c
  fromEnum GDK_KEY_dead_invertedbreve = 0xfe6d
  fromEnum GDK_KEY_dead_belowcomma = 0xfe6e
  fromEnum GDK_KEY_dead_currency = 0xfe6f
  fromEnum GDK_KEY_dead_a = 0xfe80
  fromEnum GDK_KEY_dead_A = 0xfe81
  fromEnum GDK_KEY_dead_e = 0xfe82
  fromEnum GDK_KEY_dead_E = 0xfe83
  fromEnum GDK_KEY_dead_i = 0xfe84
  fromEnum GDK_KEY_dead_I = 0xfe85
  fromEnum GDK_KEY_dead_o = 0xfe86
  fromEnum GDK_KEY_dead_O = 0xfe87
  fromEnum GDK_KEY_dead_u = 0xfe88
  fromEnum GDK_KEY_dead_U = 0xfe89
  fromEnum GDK_KEY_dead_small_schwa = 0xfe8a
  fromEnum GDK_KEY_dead_capital_schwa = 0xfe8b
  fromEnum GDK_KEY_dead_greek = 0xfe8c
  fromEnum GDK_KEY_First_Virtual_Screen = 0xfed0
  fromEnum GDK_KEY_Prev_Virtual_Screen = 0xfed1
  fromEnum GDK_KEY_Next_Virtual_Screen = 0xfed2
  fromEnum GDK_KEY_Last_Virtual_Screen = 0xfed4
  fromEnum GDK_KEY_Terminate_Server = 0xfed5
  fromEnum GDK_KEY_AccessX_Enable = 0xfe70
  fromEnum GDK_KEY_AccessX_Feedback_Enable = 0xfe71
  fromEnum GDK_KEY_RepeatKeys_Enable = 0xfe72
  fromEnum GDK_KEY_SlowKeys_Enable = 0xfe73
  fromEnum GDK_KEY_BounceKeys_Enable = 0xfe74
  fromEnum GDK_KEY_StickyKeys_Enable = 0xfe75
  fromEnum GDK_KEY_MouseKeys_Enable = 0xfe76
  fromEnum GDK_KEY_MouseKeys_Accel_Enable = 0xfe77
  fromEnum GDK_KEY_Overlay1_Enable = 0xfe78
  fromEnum GDK_KEY_Overlay2_Enable = 0xfe79
  fromEnum GDK_KEY_AudibleBell_Enable = 0xfe7a
  fromEnum GDK_KEY_Pointer_Left = 0xfee0
  fromEnum GDK_KEY_Pointer_Right = 0xfee1
  fromEnum GDK_KEY_Pointer_Up = 0xfee2
  fromEnum GDK_KEY_Pointer_Down = 0xfee3
  fromEnum GDK_KEY_Pointer_UpLeft = 0xfee4
  fromEnum GDK_KEY_Pointer_UpRight = 0xfee5
  fromEnum GDK_KEY_Pointer_DownLeft = 0xfee6
  fromEnum GDK_KEY_Pointer_DownRight = 0xfee7
  fromEnum GDK_KEY_Pointer_Button_Dflt = 0xfee8
  fromEnum GDK_KEY_Pointer_Button1 = 0xfee9
  fromEnum GDK_KEY_Pointer_Button2 = 0xfeea
  fromEnum GDK_KEY_Pointer_Button3 = 0xfeeb
  fromEnum GDK_KEY_Pointer_Button4 = 0xfeec
  fromEnum GDK_KEY_Pointer_Button5 = 0xfeed
  fromEnum GDK_KEY_Pointer_DblClick_Dflt = 0xfeee
  fromEnum GDK_KEY_Pointer_DblClick1 = 0xfeef
  fromEnum GDK_KEY_Pointer_DblClick2 = 0xfef0
  fromEnum GDK_KEY_Pointer_DblClick3 = 0xfef1
  fromEnum GDK_KEY_Pointer_DblClick4 = 0xfef2
  fromEnum GDK_KEY_Pointer_DblClick5 = 0xfef3
  fromEnum GDK_KEY_Pointer_Drag_Dflt = 0xfef4
  fromEnum GDK_KEY_Pointer_Drag1 = 0xfef5
  fromEnum GDK_KEY_Pointer_Drag2 = 0xfef6
  fromEnum GDK_KEY_Pointer_Drag3 = 0xfef7
  fromEnum GDK_KEY_Pointer_Drag4 = 0xfef8
  fromEnum GDK_KEY_Pointer_Drag5 = 0xfefd
  fromEnum GDK_KEY_Pointer_EnableKeys = 0xfef9
  fromEnum GDK_KEY_Pointer_Accelerate = 0xfefa
  fromEnum GDK_KEY_Pointer_DfltBtnNext = 0xfefb
  fromEnum GDK_KEY_Pointer_DfltBtnPrev = 0xfefc
  fromEnum GDK_KEY_ch = 0xfea0
  fromEnum GDK_KEY_Ch = 0xfea1
  fromEnum GDK_KEY_CH = 0xfea2
  fromEnum GDK_KEY_c_h = 0xfea3
  fromEnum GDK_KEY_C_h = 0xfea4
  fromEnum GDK_KEY_C_H = 0xfea5
  fromEnum GDK_KEY_3270_Duplicate = 0xfd01
  fromEnum GDK_KEY_3270_FieldMark = 0xfd02
  fromEnum GDK_KEY_3270_Right2 = 0xfd03
  fromEnum GDK_KEY_3270_Left2 = 0xfd04
  fromEnum GDK_KEY_3270_BackTab = 0xfd05
  fromEnum GDK_KEY_3270_EraseEOF = 0xfd06
  fromEnum GDK_KEY_3270_EraseInput = 0xfd07
  fromEnum GDK_KEY_3270_Reset = 0xfd08
  fromEnum GDK_KEY_3270_Quit = 0xfd09
  fromEnum GDK_KEY_3270_PA1 = 0xfd0a
  fromEnum GDK_KEY_3270_PA2 = 0xfd0b
  fromEnum GDK_KEY_3270_PA3 = 0xfd0c
  fromEnum GDK_KEY_3270_Test = 0xfd0d
  fromEnum GDK_KEY_3270_Attn = 0xfd0e
  fromEnum GDK_KEY_3270_CursorBlink = 0xfd0f
  fromEnum GDK_KEY_3270_AltCursor = 0xfd10
  fromEnum GDK_KEY_3270_KeyClick = 0xfd11
  fromEnum GDK_KEY_3270_Jump = 0xfd12
  fromEnum GDK_KEY_3270_Ident = 0xfd13
  fromEnum GDK_KEY_3270_Rule = 0xfd14
  fromEnum GDK_KEY_3270_Copy = 0xfd15
  fromEnum GDK_KEY_3270_Play = 0xfd16
  fromEnum GDK_KEY_3270_Setup = 0xfd17
  fromEnum GDK_KEY_3270_Record = 0xfd18
  fromEnum GDK_KEY_3270_ChangeScreen = 0xfd19
  fromEnum GDK_KEY_3270_DeleteWord = 0xfd1a
  fromEnum GDK_KEY_3270_ExSelect = 0xfd1b
  fromEnum GDK_KEY_3270_CursorSelect = 0xfd1c
  fromEnum GDK_KEY_3270_PrintScreen = 0xfd1d
  fromEnum GDK_KEY_3270_Enter = 0xfd1e
  fromEnum GDK_KEY_space = 0x020
  fromEnum GDK_KEY_exclam = 0x021
  fromEnum GDK_KEY_quotedbl = 0x022
  fromEnum GDK_KEY_numbersign = 0x023
  fromEnum GDK_KEY_dollar = 0x024
  fromEnum GDK_KEY_percent = 0x025
  fromEnum GDK_KEY_ampersand = 0x026
  fromEnum GDK_KEY_apostrophe = 0x027
  fromEnum GDK_KEY_quoteright = 0x027
  fromEnum GDK_KEY_parenleft = 0x028
  fromEnum GDK_KEY_parenright = 0x029
  fromEnum GDK_KEY_asterisk = 0x02a
  fromEnum GDK_KEY_plus = 0x02b
  fromEnum GDK_KEY_comma = 0x02c
  fromEnum GDK_KEY_minus = 0x02d
  fromEnum GDK_KEY_period = 0x02e
  fromEnum GDK_KEY_slash = 0x02f
  fromEnum GDK_KEY_0 = 0x030
  fromEnum GDK_KEY_1 = 0x031
  fromEnum GDK_KEY_2 = 0x032
  fromEnum GDK_KEY_3 = 0x033
  fromEnum GDK_KEY_4 = 0x034
  fromEnum GDK_KEY_5 = 0x035
  fromEnum GDK_KEY_6 = 0x036
  fromEnum GDK_KEY_7 = 0x037
  fromEnum GDK_KEY_8 = 0x038
  fromEnum GDK_KEY_9 = 0x039
  fromEnum GDK_KEY_colon = 0x03a
  fromEnum GDK_KEY_semicolon = 0x03b
  fromEnum GDK_KEY_less = 0x03c
  fromEnum GDK_KEY_equal = 0x03d
  fromEnum GDK_KEY_greater = 0x03e
  fromEnum GDK_KEY_question = 0x03f
  fromEnum GDK_KEY_at = 0x040
  fromEnum GDK_KEY_A = 0x041
  fromEnum GDK_KEY_B = 0x042
  fromEnum GDK_KEY_C = 0x043
  fromEnum GDK_KEY_D = 0x044
  fromEnum GDK_KEY_E = 0x045
  fromEnum GDK_KEY_F = 0x046
  fromEnum GDK_KEY_G = 0x047
  fromEnum GDK_KEY_H = 0x048
  fromEnum GDK_KEY_I = 0x049
  fromEnum GDK_KEY_J = 0x04a
  fromEnum GDK_KEY_K = 0x04b
  fromEnum GDK_KEY_L = 0x04c
  fromEnum GDK_KEY_M = 0x04d
  fromEnum GDK_KEY_N = 0x04e
  fromEnum GDK_KEY_O = 0x04f
  fromEnum GDK_KEY_P = 0x050
  fromEnum GDK_KEY_Q = 0x051
  fromEnum GDK_KEY_R = 0x052
  fromEnum GDK_KEY_S = 0x053
  fromEnum GDK_KEY_T = 0x054
  fromEnum GDK_KEY_U = 0x055
  fromEnum GDK_KEY_V = 0x056
  fromEnum GDK_KEY_W = 0x057
  fromEnum GDK_KEY_X = 0x058
  fromEnum GDK_KEY_Y = 0x059
  fromEnum GDK_KEY_Z = 0x05a
  fromEnum GDK_KEY_bracketleft = 0x05b
  fromEnum GDK_KEY_backslash = 0x05c
  fromEnum GDK_KEY_bracketright = 0x05d
  fromEnum GDK_KEY_asciicircum = 0x05e
  fromEnum GDK_KEY_underscore = 0x05f
  fromEnum GDK_KEY_grave = 0x060
  fromEnum GDK_KEY_quoteleft = 0x060
  fromEnum GDK_KEY_a = 0x061
  fromEnum GDK_KEY_b = 0x062
  fromEnum GDK_KEY_c = 0x063
  fromEnum GDK_KEY_d = 0x064
  fromEnum GDK_KEY_e = 0x065
  fromEnum GDK_KEY_f = 0x066
  fromEnum GDK_KEY_g = 0x067
  fromEnum GDK_KEY_h = 0x068
  fromEnum GDK_KEY_i = 0x069
  fromEnum GDK_KEY_j = 0x06a
  fromEnum GDK_KEY_k = 0x06b
  fromEnum GDK_KEY_l = 0x06c
  fromEnum GDK_KEY_m = 0x06d
  fromEnum GDK_KEY_n = 0x06e
  fromEnum GDK_KEY_o = 0x06f
  fromEnum GDK_KEY_p = 0x070
  fromEnum GDK_KEY_q = 0x071
  fromEnum GDK_KEY_r = 0x072
  fromEnum GDK_KEY_s = 0x073
  fromEnum GDK_KEY_t = 0x074
  fromEnum GDK_KEY_u = 0x075
  fromEnum GDK_KEY_v = 0x076
  fromEnum GDK_KEY_w = 0x077
  fromEnum GDK_KEY_x = 0x078
  fromEnum GDK_KEY_y = 0x079
  fromEnum GDK_KEY_z = 0x07a
  fromEnum GDK_KEY_braceleft = 0x07b
  fromEnum GDK_KEY_bar = 0x07c
  fromEnum GDK_KEY_braceright = 0x07d
  fromEnum GDK_KEY_asciitilde = 0x07e
  fromEnum GDK_KEY_nobreakspace = 0x0a0
  fromEnum GDK_KEY_exclamdown = 0x0a1
  fromEnum GDK_KEY_cent = 0x0a2
  fromEnum GDK_KEY_sterling = 0x0a3
  fromEnum GDK_KEY_currency = 0x0a4
  fromEnum GDK_KEY_yen = 0x0a5
  fromEnum GDK_KEY_brokenbar = 0x0a6
  fromEnum GDK_KEY_section = 0x0a7
  fromEnum GDK_KEY_diaeresis = 0x0a8
  fromEnum GDK_KEY_copyright = 0x0a9
  fromEnum GDK_KEY_ordfeminine = 0x0aa
  fromEnum GDK_KEY_guillemotleft = 0x0ab
  fromEnum GDK_KEY_notsign = 0x0ac
  fromEnum GDK_KEY_hyphen = 0x0ad
  fromEnum GDK_KEY_registered = 0x0ae
  fromEnum GDK_KEY_macron = 0x0af
  fromEnum GDK_KEY_degree = 0x0b0
  fromEnum GDK_KEY_plusminus = 0x0b1
  fromEnum GDK_KEY_twosuperior = 0x0b2
  fromEnum GDK_KEY_threesuperior = 0x0b3
  fromEnum GDK_KEY_acute = 0x0b4
  fromEnum GDK_KEY_mu = 0x0b5
  fromEnum GDK_KEY_paragraph = 0x0b6
  fromEnum GDK_KEY_periodcentered = 0x0b7
  fromEnum GDK_KEY_cedilla = 0x0b8
  fromEnum GDK_KEY_onesuperior = 0x0b9
  fromEnum GDK_KEY_masculine = 0x0ba
  fromEnum GDK_KEY_guillemotright = 0x0bb
  fromEnum GDK_KEY_onequarter = 0x0bc
  fromEnum GDK_KEY_onehalf = 0x0bd
  fromEnum GDK_KEY_threequarters = 0x0be
  fromEnum GDK_KEY_questiondown = 0x0bf
  fromEnum GDK_KEY_Agrave = 0x0c0
  fromEnum GDK_KEY_Aacute = 0x0c1
  fromEnum GDK_KEY_Acircumflex = 0x0c2
  fromEnum GDK_KEY_Atilde = 0x0c3
  fromEnum GDK_KEY_Adiaeresis = 0x0c4
  fromEnum GDK_KEY_Aring = 0x0c5
  fromEnum GDK_KEY_AE = 0x0c6
  fromEnum GDK_KEY_Ccedilla = 0x0c7
  fromEnum GDK_KEY_Egrave = 0x0c8
  fromEnum GDK_KEY_Eacute = 0x0c9
  fromEnum GDK_KEY_Ecircumflex = 0x0ca
  fromEnum GDK_KEY_Ediaeresis = 0x0cb
  fromEnum GDK_KEY_Igrave = 0x0cc
  fromEnum GDK_KEY_Iacute = 0x0cd
  fromEnum GDK_KEY_Icircumflex = 0x0ce
  fromEnum GDK_KEY_Idiaeresis = 0x0cf
  fromEnum GDK_KEY_ETH = 0x0d0
  fromEnum GDK_KEY_Eth = 0x0d0
  fromEnum GDK_KEY_Ntilde = 0x0d1
  fromEnum GDK_KEY_Ograve = 0x0d2
  fromEnum GDK_KEY_Oacute = 0x0d3
  fromEnum GDK_KEY_Ocircumflex = 0x0d4
  fromEnum GDK_KEY_Otilde = 0x0d5
  fromEnum GDK_KEY_Odiaeresis = 0x0d6
  fromEnum GDK_KEY_multiply = 0x0d7
  fromEnum GDK_KEY_Oslash = 0x0d8
  fromEnum GDK_KEY_Ooblique = 0x0d8
  fromEnum GDK_KEY_Ugrave = 0x0d9
  fromEnum GDK_KEY_Uacute = 0x0da
  fromEnum GDK_KEY_Ucircumflex = 0x0db
  fromEnum GDK_KEY_Udiaeresis = 0x0dc
  fromEnum GDK_KEY_Yacute = 0x0dd
  fromEnum GDK_KEY_THORN = 0x0de
  fromEnum GDK_KEY_Thorn = 0x0de
  fromEnum GDK_KEY_ssharp = 0x0df
  fromEnum GDK_KEY_agrave = 0x0e0
  fromEnum GDK_KEY_aacute = 0x0e1
  fromEnum GDK_KEY_acircumflex = 0x0e2
  fromEnum GDK_KEY_atilde = 0x0e3
  fromEnum GDK_KEY_adiaeresis = 0x0e4
  fromEnum GDK_KEY_aring = 0x0e5
  fromEnum GDK_KEY_ae = 0x0e6
  fromEnum GDK_KEY_ccedilla = 0x0e7
  fromEnum GDK_KEY_egrave = 0x0e8
  fromEnum GDK_KEY_eacute = 0x0e9
  fromEnum GDK_KEY_ecircumflex = 0x0ea
  fromEnum GDK_KEY_ediaeresis = 0x0eb
  fromEnum GDK_KEY_igrave = 0x0ec
  fromEnum GDK_KEY_iacute = 0x0ed
  fromEnum GDK_KEY_icircumflex = 0x0ee
  fromEnum GDK_KEY_idiaeresis = 0x0ef
  fromEnum GDK_KEY_eth = 0x0f0
  fromEnum GDK_KEY_ntilde = 0x0f1
  fromEnum GDK_KEY_ograve = 0x0f2
  fromEnum GDK_KEY_oacute = 0x0f3
  fromEnum GDK_KEY_ocircumflex = 0x0f4
  fromEnum GDK_KEY_otilde = 0x0f5
  fromEnum GDK_KEY_odiaeresis = 0x0f6
  fromEnum GDK_KEY_division = 0x0f7
  fromEnum GDK_KEY_oslash = 0x0f8
  fromEnum GDK_KEY_ooblique = 0x0f8
  fromEnum GDK_KEY_ugrave = 0x0f9
  fromEnum GDK_KEY_uacute = 0x0fa
  fromEnum GDK_KEY_ucircumflex = 0x0fb
  fromEnum GDK_KEY_udiaeresis = 0x0fc
  fromEnum GDK_KEY_yacute = 0x0fd
  fromEnum GDK_KEY_thorn = 0x0fe
  fromEnum GDK_KEY_ydiaeresis = 0x0ff
  fromEnum GDK_KEY_Aogonek = 0x1a1
  fromEnum GDK_KEY_breve = 0x1a2
  fromEnum GDK_KEY_Lstroke = 0x1a3
  fromEnum GDK_KEY_Lcaron = 0x1a5
  fromEnum GDK_KEY_Sacute = 0x1a6
  fromEnum GDK_KEY_Scaron = 0x1a9
  fromEnum GDK_KEY_Scedilla = 0x1aa
  fromEnum GDK_KEY_Tcaron = 0x1ab
  fromEnum GDK_KEY_Zacute = 0x1ac
  fromEnum GDK_KEY_Zcaron = 0x1ae
  fromEnum GDK_KEY_Zabovedot = 0x1af
  fromEnum GDK_KEY_aogonek = 0x1b1
  fromEnum GDK_KEY_ogonek = 0x1b2
  fromEnum GDK_KEY_lstroke = 0x1b3
  fromEnum GDK_KEY_lcaron = 0x1b5
  fromEnum GDK_KEY_sacute = 0x1b6
  fromEnum GDK_KEY_caron = 0x1b7
  fromEnum GDK_KEY_scaron = 0x1b9
  fromEnum GDK_KEY_scedilla = 0x1ba
  fromEnum GDK_KEY_tcaron = 0x1bb
  fromEnum GDK_KEY_zacute = 0x1bc
  fromEnum GDK_KEY_doubleacute = 0x1bd
  fromEnum GDK_KEY_zcaron = 0x1be
  fromEnum GDK_KEY_zabovedot = 0x1bf
  fromEnum GDK_KEY_Racute = 0x1c0
  fromEnum GDK_KEY_Abreve = 0x1c3
  fromEnum GDK_KEY_Lacute = 0x1c5
  fromEnum GDK_KEY_Cacute = 0x1c6
  fromEnum GDK_KEY_Ccaron = 0x1c8
  fromEnum GDK_KEY_Eogonek = 0x1ca
  fromEnum GDK_KEY_Ecaron = 0x1cc
  fromEnum GDK_KEY_Dcaron = 0x1cf
  fromEnum GDK_KEY_Dstroke = 0x1d0
  fromEnum GDK_KEY_Nacute = 0x1d1
  fromEnum GDK_KEY_Ncaron = 0x1d2
  fromEnum GDK_KEY_Odoubleacute = 0x1d5
  fromEnum GDK_KEY_Rcaron = 0x1d8
  fromEnum GDK_KEY_Uring = 0x1d9
  fromEnum GDK_KEY_Udoubleacute = 0x1db
  fromEnum GDK_KEY_Tcedilla = 0x1de
  fromEnum GDK_KEY_racute = 0x1e0
  fromEnum GDK_KEY_abreve = 0x1e3
  fromEnum GDK_KEY_lacute = 0x1e5
  fromEnum GDK_KEY_cacute = 0x1e6
  fromEnum GDK_KEY_ccaron = 0x1e8
  fromEnum GDK_KEY_eogonek = 0x1ea
  fromEnum GDK_KEY_ecaron = 0x1ec
  fromEnum GDK_KEY_dcaron = 0x1ef
  fromEnum GDK_KEY_dstroke = 0x1f0
  fromEnum GDK_KEY_nacute = 0x1f1
  fromEnum GDK_KEY_ncaron = 0x1f2
  fromEnum GDK_KEY_odoubleacute = 0x1f5
  fromEnum GDK_KEY_rcaron = 0x1f8
  fromEnum GDK_KEY_uring = 0x1f9
  fromEnum GDK_KEY_udoubleacute = 0x1fb
  fromEnum GDK_KEY_tcedilla = 0x1fe
  fromEnum GDK_KEY_abovedot = 0x1ff
  fromEnum GDK_KEY_Hstroke = 0x2a1
  fromEnum GDK_KEY_Hcircumflex = 0x2a6
  fromEnum GDK_KEY_Iabovedot = 0x2a9
  fromEnum GDK_KEY_Gbreve = 0x2ab
  fromEnum GDK_KEY_Jcircumflex = 0x2ac
  fromEnum GDK_KEY_hstroke = 0x2b1
  fromEnum GDK_KEY_hcircumflex = 0x2b6
  fromEnum GDK_KEY_idotless = 0x2b9
  fromEnum GDK_KEY_gbreve = 0x2bb
  fromEnum GDK_KEY_jcircumflex = 0x2bc
  fromEnum GDK_KEY_Cabovedot = 0x2c5
  fromEnum GDK_KEY_Ccircumflex = 0x2c6
  fromEnum GDK_KEY_Gabovedot = 0x2d5
  fromEnum GDK_KEY_Gcircumflex = 0x2d8
  fromEnum GDK_KEY_Ubreve = 0x2dd
  fromEnum GDK_KEY_Scircumflex = 0x2de
  fromEnum GDK_KEY_cabovedot = 0x2e5
  fromEnum GDK_KEY_ccircumflex = 0x2e6
  fromEnum GDK_KEY_gabovedot = 0x2f5
  fromEnum GDK_KEY_gcircumflex = 0x2f8
  fromEnum GDK_KEY_ubreve = 0x2fd
  fromEnum GDK_KEY_scircumflex = 0x2fe
  fromEnum GDK_KEY_kra = 0x3a2
  fromEnum GDK_KEY_kappa = 0x3a2
  fromEnum GDK_KEY_Rcedilla = 0x3a3
  fromEnum GDK_KEY_Itilde = 0x3a5
  fromEnum GDK_KEY_Lcedilla = 0x3a6
  fromEnum GDK_KEY_Emacron = 0x3aa
  fromEnum GDK_KEY_Gcedilla = 0x3ab
  fromEnum GDK_KEY_Tslash = 0x3ac
  fromEnum GDK_KEY_rcedilla = 0x3b3
  fromEnum GDK_KEY_itilde = 0x3b5
  fromEnum GDK_KEY_lcedilla = 0x3b6
  fromEnum GDK_KEY_emacron = 0x3ba
  fromEnum GDK_KEY_gcedilla = 0x3bb
  fromEnum GDK_KEY_tslash = 0x3bc
  fromEnum GDK_KEY_ENG = 0x3bd
  fromEnum GDK_KEY_eng = 0x3bf
  fromEnum GDK_KEY_Amacron = 0x3c0
  fromEnum GDK_KEY_Iogonek = 0x3c7
  fromEnum GDK_KEY_Eabovedot = 0x3cc
  fromEnum GDK_KEY_Imacron = 0x3cf
  fromEnum GDK_KEY_Ncedilla = 0x3d1
  fromEnum GDK_KEY_Omacron = 0x3d2
  fromEnum GDK_KEY_Kcedilla = 0x3d3
  fromEnum GDK_KEY_Uogonek = 0x3d9
  fromEnum GDK_KEY_Utilde = 0x3dd
  fromEnum GDK_KEY_Umacron = 0x3de
  fromEnum GDK_KEY_amacron = 0x3e0
  fromEnum GDK_KEY_iogonek = 0x3e7
  fromEnum GDK_KEY_eabovedot = 0x3ec
  fromEnum GDK_KEY_imacron = 0x3ef
  fromEnum GDK_KEY_ncedilla = 0x3f1
  fromEnum GDK_KEY_omacron = 0x3f2
  fromEnum GDK_KEY_kcedilla = 0x3f3
  fromEnum GDK_KEY_uogonek = 0x3f9
  fromEnum GDK_KEY_utilde = 0x3fd
  fromEnum GDK_KEY_umacron = 0x3fe
  fromEnum GDK_KEY_Wcircumflex = 0x1000174
  fromEnum GDK_KEY_wcircumflex = 0x1000175
  fromEnum GDK_KEY_Ycircumflex = 0x1000176
  fromEnum GDK_KEY_ycircumflex = 0x1000177
  fromEnum GDK_KEY_Babovedot = 0x1001e02
  fromEnum GDK_KEY_babovedot = 0x1001e03
  fromEnum GDK_KEY_Dabovedot = 0x1001e0a
  fromEnum GDK_KEY_dabovedot = 0x1001e0b
  fromEnum GDK_KEY_Fabovedot = 0x1001e1e
  fromEnum GDK_KEY_fabovedot = 0x1001e1f
  fromEnum GDK_KEY_Mabovedot = 0x1001e40
  fromEnum GDK_KEY_mabovedot = 0x1001e41
  fromEnum GDK_KEY_Pabovedot = 0x1001e56
  fromEnum GDK_KEY_pabovedot = 0x1001e57
  fromEnum GDK_KEY_Sabovedot = 0x1001e60
  fromEnum GDK_KEY_sabovedot = 0x1001e61
  fromEnum GDK_KEY_Tabovedot = 0x1001e6a
  fromEnum GDK_KEY_tabovedot = 0x1001e6b
  fromEnum GDK_KEY_Wgrave = 0x1001e80
  fromEnum GDK_KEY_wgrave = 0x1001e81
  fromEnum GDK_KEY_Wacute = 0x1001e82
  fromEnum GDK_KEY_wacute = 0x1001e83
  fromEnum GDK_KEY_Wdiaeresis = 0x1001e84
  fromEnum GDK_KEY_wdiaeresis = 0x1001e85
  fromEnum GDK_KEY_Ygrave = 0x1001ef2
  fromEnum GDK_KEY_ygrave = 0x1001ef3
  fromEnum GDK_KEY_OE = 0x13bc
  fromEnum GDK_KEY_oe = 0x13bd
  fromEnum GDK_KEY_Ydiaeresis = 0x13be
  fromEnum GDK_KEY_overline = 0x47e
  fromEnum GDK_KEY_kana_fullstop = 0x4a1
  fromEnum GDK_KEY_kana_openingbracket = 0x4a2
  fromEnum GDK_KEY_kana_closingbracket = 0x4a3
  fromEnum GDK_KEY_kana_comma = 0x4a4
  fromEnum GDK_KEY_kana_conjunctive = 0x4a5
  fromEnum GDK_KEY_kana_middledot = 0x4a5
  fromEnum GDK_KEY_kana_WO = 0x4a6
  fromEnum GDK_KEY_kana_a = 0x4a7
  fromEnum GDK_KEY_kana_i = 0x4a8
  fromEnum GDK_KEY_kana_u = 0x4a9
  fromEnum GDK_KEY_kana_e = 0x4aa
  fromEnum GDK_KEY_kana_o = 0x4ab
  fromEnum GDK_KEY_kana_ya = 0x4ac
  fromEnum GDK_KEY_kana_yu = 0x4ad
  fromEnum GDK_KEY_kana_yo = 0x4ae
  fromEnum GDK_KEY_kana_tsu = 0x4af
  fromEnum GDK_KEY_kana_tu = 0x4af
  fromEnum GDK_KEY_prolongedsound = 0x4b0
  fromEnum GDK_KEY_kana_A = 0x4b1
  fromEnum GDK_KEY_kana_I = 0x4b2
  fromEnum GDK_KEY_kana_U = 0x4b3
  fromEnum GDK_KEY_kana_E = 0x4b4
  fromEnum GDK_KEY_kana_O = 0x4b5
  fromEnum GDK_KEY_kana_KA = 0x4b6
  fromEnum GDK_KEY_kana_KI = 0x4b7
  fromEnum GDK_KEY_kana_KU = 0x4b8
  fromEnum GDK_KEY_kana_KE = 0x4b9
  fromEnum GDK_KEY_kana_KO = 0x4ba
  fromEnum GDK_KEY_kana_SA = 0x4bb
  fromEnum GDK_KEY_kana_SHI = 0x4bc
  fromEnum GDK_KEY_kana_SU = 0x4bd
  fromEnum GDK_KEY_kana_SE = 0x4be
  fromEnum GDK_KEY_kana_SO = 0x4bf
  fromEnum GDK_KEY_kana_TA = 0x4c0
  fromEnum GDK_KEY_kana_CHI = 0x4c1
  fromEnum GDK_KEY_kana_TI = 0x4c1
  fromEnum GDK_KEY_kana_TSU = 0x4c2
  fromEnum GDK_KEY_kana_TU = 0x4c2
  fromEnum GDK_KEY_kana_TE = 0x4c3
  fromEnum GDK_KEY_kana_TO = 0x4c4
  fromEnum GDK_KEY_kana_NA = 0x4c5
  fromEnum GDK_KEY_kana_NI = 0x4c6
  fromEnum GDK_KEY_kana_NU = 0x4c7
  fromEnum GDK_KEY_kana_NE = 0x4c8
  fromEnum GDK_KEY_kana_NO = 0x4c9
  fromEnum GDK_KEY_kana_HA = 0x4ca
  fromEnum GDK_KEY_kana_HI = 0x4cb
  fromEnum GDK_KEY_kana_FU = 0x4cc
  fromEnum GDK_KEY_kana_HU = 0x4cc
  fromEnum GDK_KEY_kana_HE = 0x4cd
  fromEnum GDK_KEY_kana_HO = 0x4ce
  fromEnum GDK_KEY_kana_MA = 0x4cf
  fromEnum GDK_KEY_kana_MI = 0x4d0
  fromEnum GDK_KEY_kana_MU = 0x4d1
  fromEnum GDK_KEY_kana_ME = 0x4d2
  fromEnum GDK_KEY_kana_MO = 0x4d3
  fromEnum GDK_KEY_kana_YA = 0x4d4
  fromEnum GDK_KEY_kana_YU = 0x4d5
  fromEnum GDK_KEY_kana_YO = 0x4d6
  fromEnum GDK_KEY_kana_RA = 0x4d7
  fromEnum GDK_KEY_kana_RI = 0x4d8
  fromEnum GDK_KEY_kana_RU = 0x4d9
  fromEnum GDK_KEY_kana_RE = 0x4da
  fromEnum GDK_KEY_kana_RO = 0x4db
  fromEnum GDK_KEY_kana_WA = 0x4dc
  fromEnum GDK_KEY_kana_N = 0x4dd
  fromEnum GDK_KEY_voicedsound = 0x4de
  fromEnum GDK_KEY_semivoicedsound = 0x4df
  fromEnum GDK_KEY_kana_switch = 0xff7e
  fromEnum GDK_KEY_Farsi_0 = 0x10006f0
  fromEnum GDK_KEY_Farsi_1 = 0x10006f1
  fromEnum GDK_KEY_Farsi_2 = 0x10006f2
  fromEnum GDK_KEY_Farsi_3 = 0x10006f3
  fromEnum GDK_KEY_Farsi_4 = 0x10006f4
  fromEnum GDK_KEY_Farsi_5 = 0x10006f5
  fromEnum GDK_KEY_Farsi_6 = 0x10006f6
  fromEnum GDK_KEY_Farsi_7 = 0x10006f7
  fromEnum GDK_KEY_Farsi_8 = 0x10006f8
  fromEnum GDK_KEY_Farsi_9 = 0x10006f9
  fromEnum GDK_KEY_Arabic_percent = 0x100066a
  fromEnum GDK_KEY_Arabic_superscript_alef = 0x1000670
  fromEnum GDK_KEY_Arabic_tteh = 0x1000679
  fromEnum GDK_KEY_Arabic_peh = 0x100067e
  fromEnum GDK_KEY_Arabic_tcheh = 0x1000686
  fromEnum GDK_KEY_Arabic_ddal = 0x1000688
  fromEnum GDK_KEY_Arabic_rreh = 0x1000691
  fromEnum GDK_KEY_Arabic_comma = 0x5ac
  fromEnum GDK_KEY_Arabic_fullstop = 0x10006d4
  fromEnum GDK_KEY_Arabic_0 = 0x1000660
  fromEnum GDK_KEY_Arabic_1 = 0x1000661
  fromEnum GDK_KEY_Arabic_2 = 0x1000662
  fromEnum GDK_KEY_Arabic_3 = 0x1000663
  fromEnum GDK_KEY_Arabic_4 = 0x1000664
  fromEnum GDK_KEY_Arabic_5 = 0x1000665
  fromEnum GDK_KEY_Arabic_6 = 0x1000666
  fromEnum GDK_KEY_Arabic_7 = 0x1000667
  fromEnum GDK_KEY_Arabic_8 = 0x1000668
  fromEnum GDK_KEY_Arabic_9 = 0x1000669
  fromEnum GDK_KEY_Arabic_semicolon = 0x5bb
  fromEnum GDK_KEY_Arabic_question_mark = 0x5bf
  fromEnum GDK_KEY_Arabic_hamza = 0x5c1
  fromEnum GDK_KEY_Arabic_maddaonalef = 0x5c2
  fromEnum GDK_KEY_Arabic_hamzaonalef = 0x5c3
  fromEnum GDK_KEY_Arabic_hamzaonwaw = 0x5c4
  fromEnum GDK_KEY_Arabic_hamzaunderalef = 0x5c5
  fromEnum GDK_KEY_Arabic_hamzaonyeh = 0x5c6
  fromEnum GDK_KEY_Arabic_alef = 0x5c7
  fromEnum GDK_KEY_Arabic_beh = 0x5c8
  fromEnum GDK_KEY_Arabic_tehmarbuta = 0x5c9
  fromEnum GDK_KEY_Arabic_teh = 0x5ca
  fromEnum GDK_KEY_Arabic_theh = 0x5cb
  fromEnum GDK_KEY_Arabic_jeem = 0x5cc
  fromEnum GDK_KEY_Arabic_hah = 0x5cd
  fromEnum GDK_KEY_Arabic_khah = 0x5ce
  fromEnum GDK_KEY_Arabic_dal = 0x5cf
  fromEnum GDK_KEY_Arabic_thal = 0x5d0
  fromEnum GDK_KEY_Arabic_ra = 0x5d1
  fromEnum GDK_KEY_Arabic_zain = 0x5d2
  fromEnum GDK_KEY_Arabic_seen = 0x5d3
  fromEnum GDK_KEY_Arabic_sheen = 0x5d4
  fromEnum GDK_KEY_Arabic_sad = 0x5d5
  fromEnum GDK_KEY_Arabic_dad = 0x5d6
  fromEnum GDK_KEY_Arabic_tah = 0x5d7
  fromEnum GDK_KEY_Arabic_zah = 0x5d8
  fromEnum GDK_KEY_Arabic_ain = 0x5d9
  fromEnum GDK_KEY_Arabic_ghain = 0x5da
  fromEnum GDK_KEY_Arabic_tatweel = 0x5e0
  fromEnum GDK_KEY_Arabic_feh = 0x5e1
  fromEnum GDK_KEY_Arabic_qaf = 0x5e2
  fromEnum GDK_KEY_Arabic_kaf = 0x5e3
  fromEnum GDK_KEY_Arabic_lam = 0x5e4
  fromEnum GDK_KEY_Arabic_meem = 0x5e5
  fromEnum GDK_KEY_Arabic_noon = 0x5e6
  fromEnum GDK_KEY_Arabic_ha = 0x5e7
  fromEnum GDK_KEY_Arabic_heh = 0x5e7
  fromEnum GDK_KEY_Arabic_waw = 0x5e8
  fromEnum GDK_KEY_Arabic_alefmaksura = 0x5e9
  fromEnum GDK_KEY_Arabic_yeh = 0x5ea
  fromEnum GDK_KEY_Arabic_fathatan = 0x5eb
  fromEnum GDK_KEY_Arabic_dammatan = 0x5ec
  fromEnum GDK_KEY_Arabic_kasratan = 0x5ed
  fromEnum GDK_KEY_Arabic_fatha = 0x5ee
  fromEnum GDK_KEY_Arabic_damma = 0x5ef
  fromEnum GDK_KEY_Arabic_kasra = 0x5f0
  fromEnum GDK_KEY_Arabic_shadda = 0x5f1
  fromEnum GDK_KEY_Arabic_sukun = 0x5f2
  fromEnum GDK_KEY_Arabic_madda_above = 0x1000653
  fromEnum GDK_KEY_Arabic_hamza_above = 0x1000654
  fromEnum GDK_KEY_Arabic_hamza_below = 0x1000655
  fromEnum GDK_KEY_Arabic_jeh = 0x1000698
  fromEnum GDK_KEY_Arabic_veh = 0x10006a4
  fromEnum GDK_KEY_Arabic_keheh = 0x10006a9
  fromEnum GDK_KEY_Arabic_gaf = 0x10006af
  fromEnum GDK_KEY_Arabic_noon_ghunna = 0x10006ba
  fromEnum GDK_KEY_Arabic_heh_doachashmee = 0x10006be
  fromEnum GDK_KEY_Farsi_yeh = 0x10006cc
  fromEnum GDK_KEY_Arabic_farsi_yeh = 0x10006cc
  fromEnum GDK_KEY_Arabic_yeh_baree = 0x10006d2
  fromEnum GDK_KEY_Arabic_heh_goal = 0x10006c1
  fromEnum GDK_KEY_Arabic_switch = 0xff7e
  fromEnum GDK_KEY_Cyrillic_GHE_bar = 0x1000492
  fromEnum GDK_KEY_Cyrillic_ghe_bar = 0x1000493
  fromEnum GDK_KEY_Cyrillic_ZHE_descender = 0x1000496
  fromEnum GDK_KEY_Cyrillic_zhe_descender = 0x1000497
  fromEnum GDK_KEY_Cyrillic_KA_descender = 0x100049a
  fromEnum GDK_KEY_Cyrillic_ka_descender = 0x100049b
  fromEnum GDK_KEY_Cyrillic_KA_vertstroke = 0x100049c
  fromEnum GDK_KEY_Cyrillic_ka_vertstroke = 0x100049d
  fromEnum GDK_KEY_Cyrillic_EN_descender = 0x10004a2
  fromEnum GDK_KEY_Cyrillic_en_descender = 0x10004a3
  fromEnum GDK_KEY_Cyrillic_U_straight = 0x10004ae
  fromEnum GDK_KEY_Cyrillic_u_straight = 0x10004af
  fromEnum GDK_KEY_Cyrillic_U_straight_bar = 0x10004b0
  fromEnum GDK_KEY_Cyrillic_u_straight_bar = 0x10004b1
  fromEnum GDK_KEY_Cyrillic_HA_descender = 0x10004b2
  fromEnum GDK_KEY_Cyrillic_ha_descender = 0x10004b3
  fromEnum GDK_KEY_Cyrillic_CHE_descender = 0x10004b6
  fromEnum GDK_KEY_Cyrillic_che_descender = 0x10004b7
  fromEnum GDK_KEY_Cyrillic_CHE_vertstroke = 0x10004b8
  fromEnum GDK_KEY_Cyrillic_che_vertstroke = 0x10004b9
  fromEnum GDK_KEY_Cyrillic_SHHA = 0x10004ba
  fromEnum GDK_KEY_Cyrillic_shha = 0x10004bb
  fromEnum GDK_KEY_Cyrillic_SCHWA = 0x10004d8
  fromEnum GDK_KEY_Cyrillic_schwa = 0x10004d9
  fromEnum GDK_KEY_Cyrillic_I_macron = 0x10004e2
  fromEnum GDK_KEY_Cyrillic_i_macron = 0x10004e3
  fromEnum GDK_KEY_Cyrillic_O_bar = 0x10004e8
  fromEnum GDK_KEY_Cyrillic_o_bar = 0x10004e9
  fromEnum GDK_KEY_Cyrillic_U_macron = 0x10004ee
  fromEnum GDK_KEY_Cyrillic_u_macron = 0x10004ef
  fromEnum GDK_KEY_Serbian_dje = 0x6a1
  fromEnum GDK_KEY_Macedonia_gje = 0x6a2
  fromEnum GDK_KEY_Cyrillic_io = 0x6a3
  fromEnum GDK_KEY_Ukrainian_ie = 0x6a4
  fromEnum GDK_KEY_Ukranian_je = 0x6a4
  fromEnum GDK_KEY_Macedonia_dse = 0x6a5
  fromEnum GDK_KEY_Ukrainian_i = 0x6a6
  fromEnum GDK_KEY_Ukranian_i = 0x6a6
  fromEnum GDK_KEY_Ukrainian_yi = 0x6a7
  fromEnum GDK_KEY_Ukranian_yi = 0x6a7
  fromEnum GDK_KEY_Cyrillic_je = 0x6a8
  fromEnum GDK_KEY_Serbian_je = 0x6a8
  fromEnum GDK_KEY_Cyrillic_lje = 0x6a9
  fromEnum GDK_KEY_Serbian_lje = 0x6a9
  fromEnum GDK_KEY_Cyrillic_nje = 0x6aa
  fromEnum GDK_KEY_Serbian_nje = 0x6aa
  fromEnum GDK_KEY_Serbian_tshe = 0x6ab
  fromEnum GDK_KEY_Macedonia_kje = 0x6ac
  fromEnum GDK_KEY_Ukrainian_ghe_with_upturn = 0x6ad
  fromEnum GDK_KEY_Byelorussian_shortu = 0x6ae
  fromEnum GDK_KEY_Cyrillic_dzhe = 0x6af
  fromEnum GDK_KEY_Serbian_dze = 0x6af
  fromEnum GDK_KEY_numerosign = 0x6b0
  fromEnum GDK_KEY_Serbian_DJE = 0x6b1
  fromEnum GDK_KEY_Macedonia_GJE = 0x6b2
  fromEnum GDK_KEY_Cyrillic_IO = 0x6b3
  fromEnum GDK_KEY_Ukrainian_IE = 0x6b4
  fromEnum GDK_KEY_Ukranian_JE = 0x6b4
  fromEnum GDK_KEY_Macedonia_DSE = 0x6b5
  fromEnum GDK_KEY_Ukrainian_I = 0x6b6
  fromEnum GDK_KEY_Ukranian_I = 0x6b6
  fromEnum GDK_KEY_Ukrainian_YI = 0x6b7
  fromEnum GDK_KEY_Ukranian_YI = 0x6b7
  fromEnum GDK_KEY_Cyrillic_JE = 0x6b8
  fromEnum GDK_KEY_Serbian_JE = 0x6b8
  fromEnum GDK_KEY_Cyrillic_LJE = 0x6b9
  fromEnum GDK_KEY_Serbian_LJE = 0x6b9
  fromEnum GDK_KEY_Cyrillic_NJE = 0x6ba
  fromEnum GDK_KEY_Serbian_NJE = 0x6ba
  fromEnum GDK_KEY_Serbian_TSHE = 0x6bb
  fromEnum GDK_KEY_Macedonia_KJE = 0x6bc
  fromEnum GDK_KEY_Ukrainian_GHE_WITH_UPTURN = 0x6bd
  fromEnum GDK_KEY_Byelorussian_SHORTU = 0x6be
  fromEnum GDK_KEY_Cyrillic_DZHE = 0x6bf
  fromEnum GDK_KEY_Serbian_DZE = 0x6bf
  fromEnum GDK_KEY_Cyrillic_yu = 0x6c0
  fromEnum GDK_KEY_Cyrillic_a = 0x6c1
  fromEnum GDK_KEY_Cyrillic_be = 0x6c2
  fromEnum GDK_KEY_Cyrillic_tse = 0x6c3
  fromEnum GDK_KEY_Cyrillic_de = 0x6c4
  fromEnum GDK_KEY_Cyrillic_ie = 0x6c5
  fromEnum GDK_KEY_Cyrillic_ef = 0x6c6
  fromEnum GDK_KEY_Cyrillic_ghe = 0x6c7
  fromEnum GDK_KEY_Cyrillic_ha = 0x6c8
  fromEnum GDK_KEY_Cyrillic_i = 0x6c9
  fromEnum GDK_KEY_Cyrillic_shorti = 0x6ca
  fromEnum GDK_KEY_Cyrillic_ka = 0x6cb
  fromEnum GDK_KEY_Cyrillic_el = 0x6cc
  fromEnum GDK_KEY_Cyrillic_em = 0x6cd
  fromEnum GDK_KEY_Cyrillic_en = 0x6ce
  fromEnum GDK_KEY_Cyrillic_o = 0x6cf
  fromEnum GDK_KEY_Cyrillic_pe = 0x6d0
  fromEnum GDK_KEY_Cyrillic_ya = 0x6d1
  fromEnum GDK_KEY_Cyrillic_er = 0x6d2
  fromEnum GDK_KEY_Cyrillic_es = 0x6d3
  fromEnum GDK_KEY_Cyrillic_te = 0x6d4
  fromEnum GDK_KEY_Cyrillic_u = 0x6d5
  fromEnum GDK_KEY_Cyrillic_zhe = 0x6d6
  fromEnum GDK_KEY_Cyrillic_ve = 0x6d7
  fromEnum GDK_KEY_Cyrillic_softsign = 0x6d8
  fromEnum GDK_KEY_Cyrillic_yeru = 0x6d9
  fromEnum GDK_KEY_Cyrillic_ze = 0x6da
  fromEnum GDK_KEY_Cyrillic_sha = 0x6db
  fromEnum GDK_KEY_Cyrillic_e = 0x6dc
  fromEnum GDK_KEY_Cyrillic_shcha = 0x6dd
  fromEnum GDK_KEY_Cyrillic_che = 0x6de
  fromEnum GDK_KEY_Cyrillic_hardsign = 0x6df
  fromEnum GDK_KEY_Cyrillic_YU = 0x6e0
  fromEnum GDK_KEY_Cyrillic_A = 0x6e1
  fromEnum GDK_KEY_Cyrillic_BE = 0x6e2
  fromEnum GDK_KEY_Cyrillic_TSE = 0x6e3
  fromEnum GDK_KEY_Cyrillic_DE = 0x6e4
  fromEnum GDK_KEY_Cyrillic_IE = 0x6e5
  fromEnum GDK_KEY_Cyrillic_EF = 0x6e6
  fromEnum GDK_KEY_Cyrillic_GHE = 0x6e7
  fromEnum GDK_KEY_Cyrillic_HA = 0x6e8
  fromEnum GDK_KEY_Cyrillic_I = 0x6e9
  fromEnum GDK_KEY_Cyrillic_SHORTI = 0x6ea
  fromEnum GDK_KEY_Cyrillic_KA = 0x6eb
  fromEnum GDK_KEY_Cyrillic_EL = 0x6ec
  fromEnum GDK_KEY_Cyrillic_EM = 0x6ed
  fromEnum GDK_KEY_Cyrillic_EN = 0x6ee
  fromEnum GDK_KEY_Cyrillic_O = 0x6ef
  fromEnum GDK_KEY_Cyrillic_PE = 0x6f0
  fromEnum GDK_KEY_Cyrillic_YA = 0x6f1
  fromEnum GDK_KEY_Cyrillic_ER = 0x6f2
  fromEnum GDK_KEY_Cyrillic_ES = 0x6f3
  fromEnum GDK_KEY_Cyrillic_TE = 0x6f4
  fromEnum GDK_KEY_Cyrillic_U = 0x6f5
  fromEnum GDK_KEY_Cyrillic_ZHE = 0x6f6
  fromEnum GDK_KEY_Cyrillic_VE = 0x6f7
  fromEnum GDK_KEY_Cyrillic_SOFTSIGN = 0x6f8
  fromEnum GDK_KEY_Cyrillic_YERU = 0x6f9
  fromEnum GDK_KEY_Cyrillic_ZE = 0x6fa
  fromEnum GDK_KEY_Cyrillic_SHA = 0x6fb
  fromEnum GDK_KEY_Cyrillic_E = 0x6fc
  fromEnum GDK_KEY_Cyrillic_SHCHA = 0x6fd
  fromEnum GDK_KEY_Cyrillic_CHE = 0x6fe
  fromEnum GDK_KEY_Cyrillic_HARDSIGN = 0x6ff
  fromEnum GDK_KEY_Greek_ALPHAaccent = 0x7a1
  fromEnum GDK_KEY_Greek_EPSILONaccent = 0x7a2
  fromEnum GDK_KEY_Greek_ETAaccent = 0x7a3
  fromEnum GDK_KEY_Greek_IOTAaccent = 0x7a4
  fromEnum GDK_KEY_Greek_IOTAdieresis = 0x7a5
  fromEnum GDK_KEY_Greek_IOTAdiaeresis = 0x7a5
  fromEnum GDK_KEY_Greek_OMICRONaccent = 0x7a7
  fromEnum GDK_KEY_Greek_UPSILONaccent = 0x7a8
  fromEnum GDK_KEY_Greek_UPSILONdieresis = 0x7a9
  fromEnum GDK_KEY_Greek_OMEGAaccent = 0x7ab
  fromEnum GDK_KEY_Greek_accentdieresis = 0x7ae
  fromEnum GDK_KEY_Greek_horizbar = 0x7af
  fromEnum GDK_KEY_Greek_alphaaccent = 0x7b1
  fromEnum GDK_KEY_Greek_epsilonaccent = 0x7b2
  fromEnum GDK_KEY_Greek_etaaccent = 0x7b3
  fromEnum GDK_KEY_Greek_iotaaccent = 0x7b4
  fromEnum GDK_KEY_Greek_iotadieresis = 0x7b5
  fromEnum GDK_KEY_Greek_iotaaccentdieresis = 0x7b6
  fromEnum GDK_KEY_Greek_omicronaccent = 0x7b7
  fromEnum GDK_KEY_Greek_upsilonaccent = 0x7b8
  fromEnum GDK_KEY_Greek_upsilondieresis = 0x7b9
  fromEnum GDK_KEY_Greek_upsilonaccentdieresis = 0x7ba
  fromEnum GDK_KEY_Greek_omegaaccent = 0x7bb
  fromEnum GDK_KEY_Greek_ALPHA = 0x7c1
  fromEnum GDK_KEY_Greek_BETA = 0x7c2
  fromEnum GDK_KEY_Greek_GAMMA = 0x7c3
  fromEnum GDK_KEY_Greek_DELTA = 0x7c4
  fromEnum GDK_KEY_Greek_EPSILON = 0x7c5
  fromEnum GDK_KEY_Greek_ZETA = 0x7c6
  fromEnum GDK_KEY_Greek_ETA = 0x7c7
  fromEnum GDK_KEY_Greek_THETA = 0x7c8
  fromEnum GDK_KEY_Greek_IOTA = 0x7c9
  fromEnum GDK_KEY_Greek_KAPPA = 0x7ca
  fromEnum GDK_KEY_Greek_LAMDA = 0x7cb
  fromEnum GDK_KEY_Greek_LAMBDA = 0x7cb
  fromEnum GDK_KEY_Greek_MU = 0x7cc
  fromEnum GDK_KEY_Greek_NU = 0x7cd
  fromEnum GDK_KEY_Greek_XI = 0x7ce
  fromEnum GDK_KEY_Greek_OMICRON = 0x7cf
  fromEnum GDK_KEY_Greek_PI = 0x7d0
  fromEnum GDK_KEY_Greek_RHO = 0x7d1
  fromEnum GDK_KEY_Greek_SIGMA = 0x7d2
  fromEnum GDK_KEY_Greek_TAU = 0x7d4
  fromEnum GDK_KEY_Greek_UPSILON = 0x7d5
  fromEnum GDK_KEY_Greek_PHI = 0x7d6
  fromEnum GDK_KEY_Greek_CHI = 0x7d7
  fromEnum GDK_KEY_Greek_PSI = 0x7d8
  fromEnum GDK_KEY_Greek_OMEGA = 0x7d9
  fromEnum GDK_KEY_Greek_alpha = 0x7e1
  fromEnum GDK_KEY_Greek_beta = 0x7e2
  fromEnum GDK_KEY_Greek_gamma = 0x7e3
  fromEnum GDK_KEY_Greek_delta = 0x7e4
  fromEnum GDK_KEY_Greek_epsilon = 0x7e5
  fromEnum GDK_KEY_Greek_zeta = 0x7e6
  fromEnum GDK_KEY_Greek_eta = 0x7e7
  fromEnum GDK_KEY_Greek_theta = 0x7e8
  fromEnum GDK_KEY_Greek_iota = 0x7e9
  fromEnum GDK_KEY_Greek_kappa = 0x7ea
  fromEnum GDK_KEY_Greek_lamda = 0x7eb
  fromEnum GDK_KEY_Greek_lambda = 0x7eb
  fromEnum GDK_KEY_Greek_mu = 0x7ec
  fromEnum GDK_KEY_Greek_nu = 0x7ed
  fromEnum GDK_KEY_Greek_xi = 0x7ee
  fromEnum GDK_KEY_Greek_omicron = 0x7ef
  fromEnum GDK_KEY_Greek_pi = 0x7f0
  fromEnum GDK_KEY_Greek_rho = 0x7f1
  fromEnum GDK_KEY_Greek_sigma = 0x7f2
  fromEnum GDK_KEY_Greek_finalsmallsigma = 0x7f3
  fromEnum GDK_KEY_Greek_tau = 0x7f4
  fromEnum GDK_KEY_Greek_upsilon = 0x7f5
  fromEnum GDK_KEY_Greek_phi = 0x7f6
  fromEnum GDK_KEY_Greek_chi = 0x7f7
  fromEnum GDK_KEY_Greek_psi = 0x7f8
  fromEnum GDK_KEY_Greek_omega = 0x7f9
  fromEnum GDK_KEY_Greek_switch = 0xff7e
  fromEnum GDK_KEY_leftradical = 0x8a1
  fromEnum GDK_KEY_topleftradical = 0x8a2
  fromEnum GDK_KEY_horizconnector = 0x8a3
  fromEnum GDK_KEY_topintegral = 0x8a4
  fromEnum GDK_KEY_botintegral = 0x8a5
  fromEnum GDK_KEY_vertconnector = 0x8a6
  fromEnum GDK_KEY_topleftsqbracket = 0x8a7
  fromEnum GDK_KEY_botleftsqbracket = 0x8a8
  fromEnum GDK_KEY_toprightsqbracket = 0x8a9
  fromEnum GDK_KEY_botrightsqbracket = 0x8aa
  fromEnum GDK_KEY_topleftparens = 0x8ab
  fromEnum GDK_KEY_botleftparens = 0x8ac
  fromEnum GDK_KEY_toprightparens = 0x8ad
  fromEnum GDK_KEY_botrightparens = 0x8ae
  fromEnum GDK_KEY_leftmiddlecurlybrace = 0x8af
  fromEnum GDK_KEY_rightmiddlecurlybrace = 0x8b0
  fromEnum GDK_KEY_topleftsummation = 0x8b1
  fromEnum GDK_KEY_botleftsummation = 0x8b2
  fromEnum GDK_KEY_topvertsummationconnector = 0x8b3
  fromEnum GDK_KEY_botvertsummationconnector = 0x8b4
  fromEnum GDK_KEY_toprightsummation = 0x8b5
  fromEnum GDK_KEY_botrightsummation = 0x8b6
  fromEnum GDK_KEY_rightmiddlesummation = 0x8b7
  fromEnum GDK_KEY_lessthanequal = 0x8bc
  fromEnum GDK_KEY_notequal = 0x8bd
  fromEnum GDK_KEY_greaterthanequal = 0x8be
  fromEnum GDK_KEY_integral = 0x8bf
  fromEnum GDK_KEY_therefore = 0x8c0
  fromEnum GDK_KEY_variation = 0x8c1
  fromEnum GDK_KEY_infinity = 0x8c2
  fromEnum GDK_KEY_nabla = 0x8c5
  fromEnum GDK_KEY_approximate = 0x8c8
  fromEnum GDK_KEY_similarequal = 0x8c9
  fromEnum GDK_KEY_ifonlyif = 0x8cd
  fromEnum GDK_KEY_implies = 0x8ce
  fromEnum GDK_KEY_identical = 0x8cf
  fromEnum GDK_KEY_radical = 0x8d6
  fromEnum GDK_KEY_includedin = 0x8da
  fromEnum GDK_KEY_includes = 0x8db
  fromEnum GDK_KEY_intersection = 0x8dc
  fromEnum GDK_KEY_union = 0x8dd
  fromEnum GDK_KEY_logicaland = 0x8de
  fromEnum GDK_KEY_logicalor = 0x8df
  fromEnum GDK_KEY_partialderivative = 0x8ef
  fromEnum GDK_KEY_function = 0x8f6
  fromEnum GDK_KEY_leftarrow = 0x8fb
  fromEnum GDK_KEY_uparrow = 0x8fc
  fromEnum GDK_KEY_rightarrow = 0x8fd
  fromEnum GDK_KEY_downarrow = 0x8fe
  fromEnum GDK_KEY_blank = 0x9df
  fromEnum GDK_KEY_soliddiamond = 0x9e0
  fromEnum GDK_KEY_checkerboard = 0x9e1
  fromEnum GDK_KEY_ht = 0x9e2
  fromEnum GDK_KEY_ff = 0x9e3
  fromEnum GDK_KEY_cr = 0x9e4
  fromEnum GDK_KEY_lf = 0x9e5
  fromEnum GDK_KEY_nl = 0x9e8
  fromEnum GDK_KEY_vt = 0x9e9
  fromEnum GDK_KEY_lowrightcorner = 0x9ea
  fromEnum GDK_KEY_uprightcorner = 0x9eb
  fromEnum GDK_KEY_upleftcorner = 0x9ec
  fromEnum GDK_KEY_lowleftcorner = 0x9ed
  fromEnum GDK_KEY_crossinglines = 0x9ee
  fromEnum GDK_KEY_horizlinescan1 = 0x9ef
  fromEnum GDK_KEY_horizlinescan3 = 0x9f0
  fromEnum GDK_KEY_horizlinescan5 = 0x9f1
  fromEnum GDK_KEY_horizlinescan7 = 0x9f2
  fromEnum GDK_KEY_horizlinescan9 = 0x9f3
  fromEnum GDK_KEY_leftt = 0x9f4
  fromEnum GDK_KEY_rightt = 0x9f5
  fromEnum GDK_KEY_bott = 0x9f6
  fromEnum GDK_KEY_topt = 0x9f7
  fromEnum GDK_KEY_vertbar = 0x9f8
  fromEnum GDK_KEY_emspace = 0xaa1
  fromEnum GDK_KEY_enspace = 0xaa2
  fromEnum GDK_KEY_em3space = 0xaa3
  fromEnum GDK_KEY_em4space = 0xaa4
  fromEnum GDK_KEY_digitspace = 0xaa5
  fromEnum GDK_KEY_punctspace = 0xaa6
  fromEnum GDK_KEY_thinspace = 0xaa7
  fromEnum GDK_KEY_hairspace = 0xaa8
  fromEnum GDK_KEY_emdash = 0xaa9
  fromEnum GDK_KEY_endash = 0xaaa
  fromEnum GDK_KEY_signifblank = 0xaac
  fromEnum GDK_KEY_ellipsis = 0xaae
  fromEnum GDK_KEY_doubbaselinedot = 0xaaf
  fromEnum GDK_KEY_onethird = 0xab0
  fromEnum GDK_KEY_twothirds = 0xab1
  fromEnum GDK_KEY_onefifth = 0xab2
  fromEnum GDK_KEY_twofifths = 0xab3
  fromEnum GDK_KEY_threefifths = 0xab4
  fromEnum GDK_KEY_fourfifths = 0xab5
  fromEnum GDK_KEY_onesixth = 0xab6
  fromEnum GDK_KEY_fivesixths = 0xab7
  fromEnum GDK_KEY_careof = 0xab8
  fromEnum GDK_KEY_figdash = 0xabb
  fromEnum GDK_KEY_leftanglebracket = 0xabc
  fromEnum GDK_KEY_decimalpoint = 0xabd
  fromEnum GDK_KEY_rightanglebracket = 0xabe
  fromEnum GDK_KEY_marker = 0xabf
  fromEnum GDK_KEY_oneeighth = 0xac3
  fromEnum GDK_KEY_threeeighths = 0xac4
  fromEnum GDK_KEY_fiveeighths = 0xac5
  fromEnum GDK_KEY_seveneighths = 0xac6
  fromEnum GDK_KEY_trademark = 0xac9
  fromEnum GDK_KEY_signaturemark = 0xaca
  fromEnum GDK_KEY_trademarkincircle = 0xacb
  fromEnum GDK_KEY_leftopentriangle = 0xacc
  fromEnum GDK_KEY_rightopentriangle = 0xacd
  fromEnum GDK_KEY_emopencircle = 0xace
  fromEnum GDK_KEY_emopenrectangle = 0xacf
  fromEnum GDK_KEY_leftsinglequotemark = 0xad0
  fromEnum GDK_KEY_rightsinglequotemark = 0xad1
  fromEnum GDK_KEY_leftdoublequotemark = 0xad2
  fromEnum GDK_KEY_rightdoublequotemark = 0xad3
  fromEnum GDK_KEY_prescription = 0xad4
  fromEnum GDK_KEY_permille = 0xad5
  fromEnum GDK_KEY_minutes = 0xad6
  fromEnum GDK_KEY_seconds = 0xad7
  fromEnum GDK_KEY_latincross = 0xad9
  fromEnum GDK_KEY_hexagram = 0xada
  fromEnum GDK_KEY_filledrectbullet = 0xadb
  fromEnum GDK_KEY_filledlefttribullet = 0xadc
  fromEnum GDK_KEY_filledrighttribullet = 0xadd
  fromEnum GDK_KEY_emfilledcircle = 0xade
  fromEnum GDK_KEY_emfilledrect = 0xadf
  fromEnum GDK_KEY_enopencircbullet = 0xae0
  fromEnum GDK_KEY_enopensquarebullet = 0xae1
  fromEnum GDK_KEY_openrectbullet = 0xae2
  fromEnum GDK_KEY_opentribulletup = 0xae3
  fromEnum GDK_KEY_opentribulletdown = 0xae4
  fromEnum GDK_KEY_openstar = 0xae5
  fromEnum GDK_KEY_enfilledcircbullet = 0xae6
  fromEnum GDK_KEY_enfilledsqbullet = 0xae7
  fromEnum GDK_KEY_filledtribulletup = 0xae8
  fromEnum GDK_KEY_filledtribulletdown = 0xae9
  fromEnum GDK_KEY_leftpointer = 0xaea
  fromEnum GDK_KEY_rightpointer = 0xaeb
  fromEnum GDK_KEY_club = 0xaec
  fromEnum GDK_KEY_diamond = 0xaed
  fromEnum GDK_KEY_heart = 0xaee
  fromEnum GDK_KEY_maltesecross = 0xaf0
  fromEnum GDK_KEY_dagger = 0xaf1
  fromEnum GDK_KEY_doubledagger = 0xaf2
  fromEnum GDK_KEY_checkmark = 0xaf3
  fromEnum GDK_KEY_ballotcross = 0xaf4
  fromEnum GDK_KEY_musicalsharp = 0xaf5
  fromEnum GDK_KEY_musicalflat = 0xaf6
  fromEnum GDK_KEY_malesymbol = 0xaf7
  fromEnum GDK_KEY_femalesymbol = 0xaf8
  fromEnum GDK_KEY_telephone = 0xaf9
  fromEnum GDK_KEY_telephonerecorder = 0xafa
  fromEnum GDK_KEY_phonographcopyright = 0xafb
  fromEnum GDK_KEY_caret = 0xafc
  fromEnum GDK_KEY_singlelowquotemark = 0xafd
  fromEnum GDK_KEY_doublelowquotemark = 0xafe
  fromEnum GDK_KEY_cursor = 0xaff
  fromEnum GDK_KEY_leftcaret = 0xba3
  fromEnum GDK_KEY_rightcaret = 0xba6
  fromEnum GDK_KEY_downcaret = 0xba8
  fromEnum GDK_KEY_upcaret = 0xba9
  fromEnum GDK_KEY_overbar = 0xbc0
  fromEnum GDK_KEY_downtack = 0xbc2
  fromEnum GDK_KEY_upshoe = 0xbc3
  fromEnum GDK_KEY_downstile = 0xbc4
  fromEnum GDK_KEY_underbar = 0xbc6
  fromEnum GDK_KEY_jot = 0xbca
  fromEnum GDK_KEY_quad = 0xbcc
  fromEnum GDK_KEY_uptack = 0xbce
  fromEnum GDK_KEY_circle = 0xbcf
  fromEnum GDK_KEY_upstile = 0xbd3
  fromEnum GDK_KEY_downshoe = 0xbd6
  fromEnum GDK_KEY_rightshoe = 0xbd8
  fromEnum GDK_KEY_leftshoe = 0xbda
  fromEnum GDK_KEY_lefttack = 0xbdc
  fromEnum GDK_KEY_righttack = 0xbfc
  fromEnum GDK_KEY_hebrew_doublelowline = 0xcdf
  fromEnum GDK_KEY_hebrew_aleph = 0xce0
  fromEnum GDK_KEY_hebrew_bet = 0xce1
  fromEnum GDK_KEY_hebrew_beth = 0xce1
  fromEnum GDK_KEY_hebrew_gimel = 0xce2
  fromEnum GDK_KEY_hebrew_gimmel = 0xce2
  fromEnum GDK_KEY_hebrew_dalet = 0xce3
  fromEnum GDK_KEY_hebrew_daleth = 0xce3
  fromEnum GDK_KEY_hebrew_he = 0xce4
  fromEnum GDK_KEY_hebrew_waw = 0xce5
  fromEnum GDK_KEY_hebrew_zain = 0xce6
  fromEnum GDK_KEY_hebrew_zayin = 0xce6
  fromEnum GDK_KEY_hebrew_chet = 0xce7
  fromEnum GDK_KEY_hebrew_het = 0xce7
  fromEnum GDK_KEY_hebrew_tet = 0xce8
  fromEnum GDK_KEY_hebrew_teth = 0xce8
  fromEnum GDK_KEY_hebrew_yod = 0xce9
  fromEnum GDK_KEY_hebrew_finalkaph = 0xcea
  fromEnum GDK_KEY_hebrew_kaph = 0xceb
  fromEnum GDK_KEY_hebrew_lamed = 0xcec
  fromEnum GDK_KEY_hebrew_finalmem = 0xced
  fromEnum GDK_KEY_hebrew_mem = 0xcee
  fromEnum GDK_KEY_hebrew_finalnun = 0xcef
  fromEnum GDK_KEY_hebrew_nun = 0xcf0
  fromEnum GDK_KEY_hebrew_samech = 0xcf1
  fromEnum GDK_KEY_hebrew_samekh = 0xcf1
  fromEnum GDK_KEY_hebrew_ayin = 0xcf2
  fromEnum GDK_KEY_hebrew_finalpe = 0xcf3
  fromEnum GDK_KEY_hebrew_pe = 0xcf4
  fromEnum GDK_KEY_hebrew_finalzade = 0xcf5
  fromEnum GDK_KEY_hebrew_finalzadi = 0xcf5
  fromEnum GDK_KEY_hebrew_zade = 0xcf6
  fromEnum GDK_KEY_hebrew_zadi = 0xcf6
  fromEnum GDK_KEY_hebrew_qoph = 0xcf7
  fromEnum GDK_KEY_hebrew_kuf = 0xcf7
  fromEnum GDK_KEY_hebrew_resh = 0xcf8
  fromEnum GDK_KEY_hebrew_shin = 0xcf9
  fromEnum GDK_KEY_hebrew_taw = 0xcfa
  fromEnum GDK_KEY_hebrew_taf = 0xcfa
  fromEnum GDK_KEY_Hebrew_switch = 0xff7e
  fromEnum GDK_KEY_Thai_kokai = 0xda1
  fromEnum GDK_KEY_Thai_khokhai = 0xda2
  fromEnum GDK_KEY_Thai_khokhuat = 0xda3
  fromEnum GDK_KEY_Thai_khokhwai = 0xda4
  fromEnum GDK_KEY_Thai_khokhon = 0xda5
  fromEnum GDK_KEY_Thai_khorakhang = 0xda6
  fromEnum GDK_KEY_Thai_ngongu = 0xda7
  fromEnum GDK_KEY_Thai_chochan = 0xda8
  fromEnum GDK_KEY_Thai_choching = 0xda9
  fromEnum GDK_KEY_Thai_chochang = 0xdaa
  fromEnum GDK_KEY_Thai_soso = 0xdab
  fromEnum GDK_KEY_Thai_chochoe = 0xdac
  fromEnum GDK_KEY_Thai_yoying = 0xdad
  fromEnum GDK_KEY_Thai_dochada = 0xdae
  fromEnum GDK_KEY_Thai_topatak = 0xdaf
  fromEnum GDK_KEY_Thai_thothan = 0xdb0
  fromEnum GDK_KEY_Thai_thonangmontho = 0xdb1
  fromEnum GDK_KEY_Thai_thophuthao = 0xdb2
  fromEnum GDK_KEY_Thai_nonen = 0xdb3
  fromEnum GDK_KEY_Thai_dodek = 0xdb4
  fromEnum GDK_KEY_Thai_totao = 0xdb5
  fromEnum GDK_KEY_Thai_thothung = 0xdb6
  fromEnum GDK_KEY_Thai_thothahan = 0xdb7
  fromEnum GDK_KEY_Thai_thothong = 0xdb8
  fromEnum GDK_KEY_Thai_nonu = 0xdb9
  fromEnum GDK_KEY_Thai_bobaimai = 0xdba
  fromEnum GDK_KEY_Thai_popla = 0xdbb
  fromEnum GDK_KEY_Thai_phophung = 0xdbc
  fromEnum GDK_KEY_Thai_fofa = 0xdbd
  fromEnum GDK_KEY_Thai_phophan = 0xdbe
  fromEnum GDK_KEY_Thai_fofan = 0xdbf
  fromEnum GDK_KEY_Thai_phosamphao = 0xdc0
  fromEnum GDK_KEY_Thai_moma = 0xdc1
  fromEnum GDK_KEY_Thai_yoyak = 0xdc2
  fromEnum GDK_KEY_Thai_rorua = 0xdc3
  fromEnum GDK_KEY_Thai_ru = 0xdc4
  fromEnum GDK_KEY_Thai_loling = 0xdc5
  fromEnum GDK_KEY_Thai_lu = 0xdc6
  fromEnum GDK_KEY_Thai_wowaen = 0xdc7
  fromEnum GDK_KEY_Thai_sosala = 0xdc8
  fromEnum GDK_KEY_Thai_sorusi = 0xdc9
  fromEnum GDK_KEY_Thai_sosua = 0xdca
  fromEnum GDK_KEY_Thai_hohip = 0xdcb
  fromEnum GDK_KEY_Thai_lochula = 0xdcc
  fromEnum GDK_KEY_Thai_oang = 0xdcd
  fromEnum GDK_KEY_Thai_honokhuk = 0xdce
  fromEnum GDK_KEY_Thai_paiyannoi = 0xdcf
  fromEnum GDK_KEY_Thai_saraa = 0xdd0
  fromEnum GDK_KEY_Thai_maihanakat = 0xdd1
  fromEnum GDK_KEY_Thai_saraaa = 0xdd2
  fromEnum GDK_KEY_Thai_saraam = 0xdd3
  fromEnum GDK_KEY_Thai_sarai = 0xdd4
  fromEnum GDK_KEY_Thai_saraii = 0xdd5
  fromEnum GDK_KEY_Thai_saraue = 0xdd6
  fromEnum GDK_KEY_Thai_sarauee = 0xdd7
  fromEnum GDK_KEY_Thai_sarau = 0xdd8
  fromEnum GDK_KEY_Thai_sarauu = 0xdd9
  fromEnum GDK_KEY_Thai_phinthu = 0xdda
  fromEnum GDK_KEY_Thai_maihanakat_maitho = 0xdde
  fromEnum GDK_KEY_Thai_baht = 0xddf
  fromEnum GDK_KEY_Thai_sarae = 0xde0
  fromEnum GDK_KEY_Thai_saraae = 0xde1
  fromEnum GDK_KEY_Thai_sarao = 0xde2
  fromEnum GDK_KEY_Thai_saraaimaimuan = 0xde3
  fromEnum GDK_KEY_Thai_saraaimaimalai = 0xde4
  fromEnum GDK_KEY_Thai_lakkhangyao = 0xde5
  fromEnum GDK_KEY_Thai_maiyamok = 0xde6
  fromEnum GDK_KEY_Thai_maitaikhu = 0xde7
  fromEnum GDK_KEY_Thai_maiek = 0xde8
  fromEnum GDK_KEY_Thai_maitho = 0xde9
  fromEnum GDK_KEY_Thai_maitri = 0xdea
  fromEnum GDK_KEY_Thai_maichattawa = 0xdeb
  fromEnum GDK_KEY_Thai_thanthakhat = 0xdec
  fromEnum GDK_KEY_Thai_nikhahit = 0xded
  fromEnum GDK_KEY_Thai_leksun = 0xdf0
  fromEnum GDK_KEY_Thai_leknung = 0xdf1
  fromEnum GDK_KEY_Thai_leksong = 0xdf2
  fromEnum GDK_KEY_Thai_leksam = 0xdf3
  fromEnum GDK_KEY_Thai_leksi = 0xdf4
  fromEnum GDK_KEY_Thai_lekha = 0xdf5
  fromEnum GDK_KEY_Thai_lekhok = 0xdf6
  fromEnum GDK_KEY_Thai_lekchet = 0xdf7
  fromEnum GDK_KEY_Thai_lekpaet = 0xdf8
  fromEnum GDK_KEY_Thai_lekkao = 0xdf9
  fromEnum GDK_KEY_Hangul = 0xff31
  fromEnum GDK_KEY_Hangul_Start = 0xff32
  fromEnum GDK_KEY_Hangul_End = 0xff33
  fromEnum GDK_KEY_Hangul_Hanja = 0xff34
  fromEnum GDK_KEY_Hangul_Jamo = 0xff35
  fromEnum GDK_KEY_Hangul_Romaja = 0xff36
  fromEnum GDK_KEY_Hangul_Codeinput = 0xff37
  fromEnum GDK_KEY_Hangul_Jeonja = 0xff38
  fromEnum GDK_KEY_Hangul_Banja = 0xff39
  fromEnum GDK_KEY_Hangul_PreHanja = 0xff3a
  fromEnum GDK_KEY_Hangul_PostHanja = 0xff3b
  fromEnum GDK_KEY_Hangul_SingleCandidate = 0xff3c
  fromEnum GDK_KEY_Hangul_MultipleCandidate = 0xff3d
  fromEnum GDK_KEY_Hangul_PreviousCandidate = 0xff3e
  fromEnum GDK_KEY_Hangul_Special = 0xff3f
  fromEnum GDK_KEY_Hangul_switch = 0xff7e
  fromEnum GDK_KEY_Hangul_Kiyeog = 0xea1
  fromEnum GDK_KEY_Hangul_SsangKiyeog = 0xea2
  fromEnum GDK_KEY_Hangul_KiyeogSios = 0xea3
  fromEnum GDK_KEY_Hangul_Nieun = 0xea4
  fromEnum GDK_KEY_Hangul_NieunJieuj = 0xea5
  fromEnum GDK_KEY_Hangul_NieunHieuh = 0xea6
  fromEnum GDK_KEY_Hangul_Dikeud = 0xea7
  fromEnum GDK_KEY_Hangul_SsangDikeud = 0xea8
  fromEnum GDK_KEY_Hangul_Rieul = 0xea9
  fromEnum GDK_KEY_Hangul_RieulKiyeog = 0xeaa
  fromEnum GDK_KEY_Hangul_RieulMieum = 0xeab
  fromEnum GDK_KEY_Hangul_RieulPieub = 0xeac
  fromEnum GDK_KEY_Hangul_RieulSios = 0xead
  fromEnum GDK_KEY_Hangul_RieulTieut = 0xeae
  fromEnum GDK_KEY_Hangul_RieulPhieuf = 0xeaf
  fromEnum GDK_KEY_Hangul_RieulHieuh = 0xeb0
  fromEnum GDK_KEY_Hangul_Mieum = 0xeb1
  fromEnum GDK_KEY_Hangul_Pieub = 0xeb2
  fromEnum GDK_KEY_Hangul_SsangPieub = 0xeb3
  fromEnum GDK_KEY_Hangul_PieubSios = 0xeb4
  fromEnum GDK_KEY_Hangul_Sios = 0xeb5
  fromEnum GDK_KEY_Hangul_SsangSios = 0xeb6
  fromEnum GDK_KEY_Hangul_Ieung = 0xeb7
  fromEnum GDK_KEY_Hangul_Jieuj = 0xeb8
  fromEnum GDK_KEY_Hangul_SsangJieuj = 0xeb9
  fromEnum GDK_KEY_Hangul_Cieuc = 0xeba
  fromEnum GDK_KEY_Hangul_Khieuq = 0xebb
  fromEnum GDK_KEY_Hangul_Tieut = 0xebc
  fromEnum GDK_KEY_Hangul_Phieuf = 0xebd
  fromEnum GDK_KEY_Hangul_Hieuh = 0xebe
  fromEnum GDK_KEY_Hangul_A = 0xebf
  fromEnum GDK_KEY_Hangul_AE = 0xec0
  fromEnum GDK_KEY_Hangul_YA = 0xec1
  fromEnum GDK_KEY_Hangul_YAE = 0xec2
  fromEnum GDK_KEY_Hangul_EO = 0xec3
  fromEnum GDK_KEY_Hangul_E = 0xec4
  fromEnum GDK_KEY_Hangul_YEO = 0xec5
  fromEnum GDK_KEY_Hangul_YE = 0xec6
  fromEnum GDK_KEY_Hangul_O = 0xec7
  fromEnum GDK_KEY_Hangul_WA = 0xec8
  fromEnum GDK_KEY_Hangul_WAE = 0xec9
  fromEnum GDK_KEY_Hangul_OE = 0xeca
  fromEnum GDK_KEY_Hangul_YO = 0xecb
  fromEnum GDK_KEY_Hangul_U = 0xecc
  fromEnum GDK_KEY_Hangul_WEO = 0xecd
  fromEnum GDK_KEY_Hangul_WE = 0xece
  fromEnum GDK_KEY_Hangul_WI = 0xecf
  fromEnum GDK_KEY_Hangul_YU = 0xed0
  fromEnum GDK_KEY_Hangul_EU = 0xed1
  fromEnum GDK_KEY_Hangul_YI = 0xed2
  fromEnum GDK_KEY_Hangul_I = 0xed3
  fromEnum GDK_KEY_Hangul_J_Kiyeog = 0xed4
  fromEnum GDK_KEY_Hangul_J_SsangKiyeog = 0xed5
  fromEnum GDK_KEY_Hangul_J_KiyeogSios = 0xed6
  fromEnum GDK_KEY_Hangul_J_Nieun = 0xed7
  fromEnum GDK_KEY_Hangul_J_NieunJieuj = 0xed8
  fromEnum GDK_KEY_Hangul_J_NieunHieuh = 0xed9
  fromEnum GDK_KEY_Hangul_J_Dikeud = 0xeda
  fromEnum GDK_KEY_Hangul_J_Rieul = 0xedb
  fromEnum GDK_KEY_Hangul_J_RieulKiyeog = 0xedc
  fromEnum GDK_KEY_Hangul_J_RieulMieum = 0xedd
  fromEnum GDK_KEY_Hangul_J_RieulPieub = 0xede
  fromEnum GDK_KEY_Hangul_J_RieulSios = 0xedf
  fromEnum GDK_KEY_Hangul_J_RieulTieut = 0xee0
  fromEnum GDK_KEY_Hangul_J_RieulPhieuf = 0xee1
  fromEnum GDK_KEY_Hangul_J_RieulHieuh = 0xee2
  fromEnum GDK_KEY_Hangul_J_Mieum = 0xee3
  fromEnum GDK_KEY_Hangul_J_Pieub = 0xee4
  fromEnum GDK_KEY_Hangul_J_PieubSios = 0xee5
  fromEnum GDK_KEY_Hangul_J_Sios = 0xee6
  fromEnum GDK_KEY_Hangul_J_SsangSios = 0xee7
  fromEnum GDK_KEY_Hangul_J_Ieung = 0xee8
  fromEnum GDK_KEY_Hangul_J_Jieuj = 0xee9
  fromEnum GDK_KEY_Hangul_J_Cieuc = 0xeea
  fromEnum GDK_KEY_Hangul_J_Khieuq = 0xeeb
  fromEnum GDK_KEY_Hangul_J_Tieut = 0xeec
  fromEnum GDK_KEY_Hangul_J_Phieuf = 0xeed
  fromEnum GDK_KEY_Hangul_J_Hieuh = 0xeee
  fromEnum GDK_KEY_Hangul_RieulYeorinHieuh = 0xeef
  fromEnum GDK_KEY_Hangul_SunkyeongeumMieum = 0xef0
  fromEnum GDK_KEY_Hangul_SunkyeongeumPieub = 0xef1
  fromEnum GDK_KEY_Hangul_PanSios = 0xef2
  fromEnum GDK_KEY_Hangul_KkogjiDalrinIeung = 0xef3
  fromEnum GDK_KEY_Hangul_SunkyeongeumPhieuf = 0xef4
  fromEnum GDK_KEY_Hangul_YeorinHieuh = 0xef5
  fromEnum GDK_KEY_Hangul_AraeA = 0xef6
  fromEnum GDK_KEY_Hangul_AraeAE = 0xef7
  fromEnum GDK_KEY_Hangul_J_PanSios = 0xef8
  fromEnum GDK_KEY_Hangul_J_KkogjiDalrinIeung = 0xef9
  fromEnum GDK_KEY_Hangul_J_YeorinHieuh = 0xefa
  fromEnum GDK_KEY_Korean_Won = 0xeff
  fromEnum GDK_KEY_Armenian_ligature_ew = 0x1000587
  fromEnum GDK_KEY_Armenian_full_stop = 0x1000589
  fromEnum GDK_KEY_Armenian_verjaket = 0x1000589
  fromEnum GDK_KEY_Armenian_separation_mark = 0x100055d
  fromEnum GDK_KEY_Armenian_but = 0x100055d
  fromEnum GDK_KEY_Armenian_hyphen = 0x100058a
  fromEnum GDK_KEY_Armenian_yentamna = 0x100058a
  fromEnum GDK_KEY_Armenian_exclam = 0x100055c
  fromEnum GDK_KEY_Armenian_amanak = 0x100055c
  fromEnum GDK_KEY_Armenian_accent = 0x100055b
  fromEnum GDK_KEY_Armenian_shesht = 0x100055b
  fromEnum GDK_KEY_Armenian_question = 0x100055e
  fromEnum GDK_KEY_Armenian_paruyk = 0x100055e
  fromEnum GDK_KEY_Armenian_AYB = 0x1000531
  fromEnum GDK_KEY_Armenian_ayb = 0x1000561
  fromEnum GDK_KEY_Armenian_BEN = 0x1000532
  fromEnum GDK_KEY_Armenian_ben = 0x1000562
  fromEnum GDK_KEY_Armenian_GIM = 0x1000533
  fromEnum GDK_KEY_Armenian_gim = 0x1000563
  fromEnum GDK_KEY_Armenian_DA = 0x1000534
  fromEnum GDK_KEY_Armenian_da = 0x1000564
  fromEnum GDK_KEY_Armenian_YECH = 0x1000535
  fromEnum GDK_KEY_Armenian_yech = 0x1000565
  fromEnum GDK_KEY_Armenian_ZA = 0x1000536
  fromEnum GDK_KEY_Armenian_za = 0x1000566
  fromEnum GDK_KEY_Armenian_E = 0x1000537
  fromEnum GDK_KEY_Armenian_e = 0x1000567
  fromEnum GDK_KEY_Armenian_AT = 0x1000538
  fromEnum GDK_KEY_Armenian_at = 0x1000568
  fromEnum GDK_KEY_Armenian_TO = 0x1000539
  fromEnum GDK_KEY_Armenian_to = 0x1000569
  fromEnum GDK_KEY_Armenian_ZHE = 0x100053a
  fromEnum GDK_KEY_Armenian_zhe = 0x100056a
  fromEnum GDK_KEY_Armenian_INI = 0x100053b
  fromEnum GDK_KEY_Armenian_ini = 0x100056b
  fromEnum GDK_KEY_Armenian_LYUN = 0x100053c
  fromEnum GDK_KEY_Armenian_lyun = 0x100056c
  fromEnum GDK_KEY_Armenian_KHE = 0x100053d
  fromEnum GDK_KEY_Armenian_khe = 0x100056d
  fromEnum GDK_KEY_Armenian_TSA = 0x100053e
  fromEnum GDK_KEY_Armenian_tsa = 0x100056e
  fromEnum GDK_KEY_Armenian_KEN = 0x100053f
  fromEnum GDK_KEY_Armenian_ken = 0x100056f
  fromEnum GDK_KEY_Armenian_HO = 0x1000540
  fromEnum GDK_KEY_Armenian_ho = 0x1000570
  fromEnum GDK_KEY_Armenian_DZA = 0x1000541
  fromEnum GDK_KEY_Armenian_dza = 0x1000571
  fromEnum GDK_KEY_Armenian_GHAT = 0x1000542
  fromEnum GDK_KEY_Armenian_ghat = 0x1000572
  fromEnum GDK_KEY_Armenian_TCHE = 0x1000543
  fromEnum GDK_KEY_Armenian_tche = 0x1000573
  fromEnum GDK_KEY_Armenian_MEN = 0x1000544
  fromEnum GDK_KEY_Armenian_men = 0x1000574
  fromEnum GDK_KEY_Armenian_HI = 0x1000545
  fromEnum GDK_KEY_Armenian_hi = 0x1000575
  fromEnum GDK_KEY_Armenian_NU = 0x1000546
  fromEnum GDK_KEY_Armenian_nu = 0x1000576
  fromEnum GDK_KEY_Armenian_SHA = 0x1000547
  fromEnum GDK_KEY_Armenian_sha = 0x1000577
  fromEnum GDK_KEY_Armenian_VO = 0x1000548
  fromEnum GDK_KEY_Armenian_vo = 0x1000578
  fromEnum GDK_KEY_Armenian_CHA = 0x1000549
  fromEnum GDK_KEY_Armenian_cha = 0x1000579
  fromEnum GDK_KEY_Armenian_PE = 0x100054a
  fromEnum GDK_KEY_Armenian_pe = 0x100057a
  fromEnum GDK_KEY_Armenian_JE = 0x100054b
  fromEnum GDK_KEY_Armenian_je = 0x100057b
  fromEnum GDK_KEY_Armenian_RA = 0x100054c
  fromEnum GDK_KEY_Armenian_ra = 0x100057c
  fromEnum GDK_KEY_Armenian_SE = 0x100054d
  fromEnum GDK_KEY_Armenian_se = 0x100057d
  fromEnum GDK_KEY_Armenian_VEV = 0x100054e
  fromEnum GDK_KEY_Armenian_vev = 0x100057e
  fromEnum GDK_KEY_Armenian_TYUN = 0x100054f
  fromEnum GDK_KEY_Armenian_tyun = 0x100057f
  fromEnum GDK_KEY_Armenian_RE = 0x1000550
  fromEnum GDK_KEY_Armenian_re = 0x1000580
  fromEnum GDK_KEY_Armenian_TSO = 0x1000551
  fromEnum GDK_KEY_Armenian_tso = 0x1000581
  fromEnum GDK_KEY_Armenian_VYUN = 0x1000552
  fromEnum GDK_KEY_Armenian_vyun = 0x1000582
  fromEnum GDK_KEY_Armenian_PYUR = 0x1000553
  fromEnum GDK_KEY_Armenian_pyur = 0x1000583
  fromEnum GDK_KEY_Armenian_KE = 0x1000554
  fromEnum GDK_KEY_Armenian_ke = 0x1000584
  fromEnum GDK_KEY_Armenian_O = 0x1000555
  fromEnum GDK_KEY_Armenian_o = 0x1000585
  fromEnum GDK_KEY_Armenian_FE = 0x1000556
  fromEnum GDK_KEY_Armenian_fe = 0x1000586
  fromEnum GDK_KEY_Armenian_apostrophe = 0x100055a
  fromEnum GDK_KEY_Georgian_an = 0x10010d0
  fromEnum GDK_KEY_Georgian_ban = 0x10010d1
  fromEnum GDK_KEY_Georgian_gan = 0x10010d2
  fromEnum GDK_KEY_Georgian_don = 0x10010d3
  fromEnum GDK_KEY_Georgian_en = 0x10010d4
  fromEnum GDK_KEY_Georgian_vin = 0x10010d5
  fromEnum GDK_KEY_Georgian_zen = 0x10010d6
  fromEnum GDK_KEY_Georgian_tan = 0x10010d7
  fromEnum GDK_KEY_Georgian_in = 0x10010d8
  fromEnum GDK_KEY_Georgian_kan = 0x10010d9
  fromEnum GDK_KEY_Georgian_las = 0x10010da
  fromEnum GDK_KEY_Georgian_man = 0x10010db
  fromEnum GDK_KEY_Georgian_nar = 0x10010dc
  fromEnum GDK_KEY_Georgian_on = 0x10010dd
  fromEnum GDK_KEY_Georgian_par = 0x10010de
  fromEnum GDK_KEY_Georgian_zhar = 0x10010df
  fromEnum GDK_KEY_Georgian_rae = 0x10010e0
  fromEnum GDK_KEY_Georgian_san = 0x10010e1
  fromEnum GDK_KEY_Georgian_tar = 0x10010e2
  fromEnum GDK_KEY_Georgian_un = 0x10010e3
  fromEnum GDK_KEY_Georgian_phar = 0x10010e4
  fromEnum GDK_KEY_Georgian_khar = 0x10010e5
  fromEnum GDK_KEY_Georgian_ghan = 0x10010e6
  fromEnum GDK_KEY_Georgian_qar = 0x10010e7
  fromEnum GDK_KEY_Georgian_shin = 0x10010e8
  fromEnum GDK_KEY_Georgian_chin = 0x10010e9
  fromEnum GDK_KEY_Georgian_can = 0x10010ea
  fromEnum GDK_KEY_Georgian_jil = 0x10010eb
  fromEnum GDK_KEY_Georgian_cil = 0x10010ec
  fromEnum GDK_KEY_Georgian_char = 0x10010ed
  fromEnum GDK_KEY_Georgian_xan = 0x10010ee
  fromEnum GDK_KEY_Georgian_jhan = 0x10010ef
  fromEnum GDK_KEY_Georgian_hae = 0x10010f0
  fromEnum GDK_KEY_Georgian_he = 0x10010f1
  fromEnum GDK_KEY_Georgian_hie = 0x10010f2
  fromEnum GDK_KEY_Georgian_we = 0x10010f3
  fromEnum GDK_KEY_Georgian_har = 0x10010f4
  fromEnum GDK_KEY_Georgian_hoe = 0x10010f5
  fromEnum GDK_KEY_Georgian_fi = 0x10010f6
  fromEnum GDK_KEY_Xabovedot = 0x1001e8a
  fromEnum GDK_KEY_Ibreve = 0x100012c
  fromEnum GDK_KEY_Zstroke = 0x10001b5
  fromEnum GDK_KEY_Gcaron = 0x10001e6
  fromEnum GDK_KEY_Ocaron = 0x10001d1
  fromEnum GDK_KEY_Obarred = 0x100019f
  fromEnum GDK_KEY_xabovedot = 0x1001e8b
  fromEnum GDK_KEY_ibreve = 0x100012d
  fromEnum GDK_KEY_zstroke = 0x10001b6
  fromEnum GDK_KEY_gcaron = 0x10001e7
  fromEnum GDK_KEY_ocaron = 0x10001d2
  fromEnum GDK_KEY_obarred = 0x1000275
  fromEnum GDK_KEY_SCHWA = 0x100018f
  fromEnum GDK_KEY_schwa = 0x1000259
  fromEnum GDK_KEY_EZH = 0x10001b7
  fromEnum GDK_KEY_ezh = 0x1000292
  fromEnum GDK_KEY_Lbelowdot = 0x1001e36
  fromEnum GDK_KEY_lbelowdot = 0x1001e37
  fromEnum GDK_KEY_Abelowdot = 0x1001ea0
  fromEnum GDK_KEY_abelowdot = 0x1001ea1
  fromEnum GDK_KEY_Ahook = 0x1001ea2
  fromEnum GDK_KEY_ahook = 0x1001ea3
  fromEnum GDK_KEY_Acircumflexacute = 0x1001ea4
  fromEnum GDK_KEY_acircumflexacute = 0x1001ea5
  fromEnum GDK_KEY_Acircumflexgrave = 0x1001ea6
  fromEnum GDK_KEY_acircumflexgrave = 0x1001ea7
  fromEnum GDK_KEY_Acircumflexhook = 0x1001ea8
  fromEnum GDK_KEY_acircumflexhook = 0x1001ea9
  fromEnum GDK_KEY_Acircumflextilde = 0x1001eaa
  fromEnum GDK_KEY_acircumflextilde = 0x1001eab
  fromEnum GDK_KEY_Acircumflexbelowdot = 0x1001eac
  fromEnum GDK_KEY_acircumflexbelowdot = 0x1001ead
  fromEnum GDK_KEY_Abreveacute = 0x1001eae
  fromEnum GDK_KEY_abreveacute = 0x1001eaf
  fromEnum GDK_KEY_Abrevegrave = 0x1001eb0
  fromEnum GDK_KEY_abrevegrave = 0x1001eb1
  fromEnum GDK_KEY_Abrevehook = 0x1001eb2
  fromEnum GDK_KEY_abrevehook = 0x1001eb3
  fromEnum GDK_KEY_Abrevetilde = 0x1001eb4
  fromEnum GDK_KEY_abrevetilde = 0x1001eb5
  fromEnum GDK_KEY_Abrevebelowdot = 0x1001eb6
  fromEnum GDK_KEY_abrevebelowdot = 0x1001eb7
  fromEnum GDK_KEY_Ebelowdot = 0x1001eb8
  fromEnum GDK_KEY_ebelowdot = 0x1001eb9
  fromEnum GDK_KEY_Ehook = 0x1001eba
  fromEnum GDK_KEY_ehook = 0x1001ebb
  fromEnum GDK_KEY_Etilde = 0x1001ebc
  fromEnum GDK_KEY_etilde = 0x1001ebd
  fromEnum GDK_KEY_Ecircumflexacute = 0x1001ebe
  fromEnum GDK_KEY_ecircumflexacute = 0x1001ebf
  fromEnum GDK_KEY_Ecircumflexgrave = 0x1001ec0
  fromEnum GDK_KEY_ecircumflexgrave = 0x1001ec1
  fromEnum GDK_KEY_Ecircumflexhook = 0x1001ec2
  fromEnum GDK_KEY_ecircumflexhook = 0x1001ec3
  fromEnum GDK_KEY_Ecircumflextilde = 0x1001ec4
  fromEnum GDK_KEY_ecircumflextilde = 0x1001ec5
  fromEnum GDK_KEY_Ecircumflexbelowdot = 0x1001ec6
  fromEnum GDK_KEY_ecircumflexbelowdot = 0x1001ec7
  fromEnum GDK_KEY_Ihook = 0x1001ec8
  fromEnum GDK_KEY_ihook = 0x1001ec9
  fromEnum GDK_KEY_Ibelowdot = 0x1001eca
  fromEnum GDK_KEY_ibelowdot = 0x1001ecb
  fromEnum GDK_KEY_Obelowdot = 0x1001ecc
  fromEnum GDK_KEY_obelowdot = 0x1001ecd
  fromEnum GDK_KEY_Ohook = 0x1001ece
  fromEnum GDK_KEY_ohook = 0x1001ecf
  fromEnum GDK_KEY_Ocircumflexacute = 0x1001ed0
  fromEnum GDK_KEY_ocircumflexacute = 0x1001ed1
  fromEnum GDK_KEY_Ocircumflexgrave = 0x1001ed2
  fromEnum GDK_KEY_ocircumflexgrave = 0x1001ed3
  fromEnum GDK_KEY_Ocircumflexhook = 0x1001ed4
  fromEnum GDK_KEY_ocircumflexhook = 0x1001ed5
  fromEnum GDK_KEY_Ocircumflextilde = 0x1001ed6
  fromEnum GDK_KEY_ocircumflextilde = 0x1001ed7
  fromEnum GDK_KEY_Ocircumflexbelowdot = 0x1001ed8
  fromEnum GDK_KEY_ocircumflexbelowdot = 0x1001ed9
  fromEnum GDK_KEY_Ohornacute = 0x1001eda
  fromEnum GDK_KEY_ohornacute = 0x1001edb
  fromEnum GDK_KEY_Ohorngrave = 0x1001edc
  fromEnum GDK_KEY_ohorngrave = 0x1001edd
  fromEnum GDK_KEY_Ohornhook = 0x1001ede
  fromEnum GDK_KEY_ohornhook = 0x1001edf
  fromEnum GDK_KEY_Ohorntilde = 0x1001ee0
  fromEnum GDK_KEY_ohorntilde = 0x1001ee1
  fromEnum GDK_KEY_Ohornbelowdot = 0x1001ee2
  fromEnum GDK_KEY_ohornbelowdot = 0x1001ee3
  fromEnum GDK_KEY_Ubelowdot = 0x1001ee4
  fromEnum GDK_KEY_ubelowdot = 0x1001ee5
  fromEnum GDK_KEY_Uhook = 0x1001ee6
  fromEnum GDK_KEY_uhook = 0x1001ee7
  fromEnum GDK_KEY_Uhornacute = 0x1001ee8
  fromEnum GDK_KEY_uhornacute = 0x1001ee9
  fromEnum GDK_KEY_Uhorngrave = 0x1001eea
  fromEnum GDK_KEY_uhorngrave = 0x1001eeb
  fromEnum GDK_KEY_Uhornhook = 0x1001eec
  fromEnum GDK_KEY_uhornhook = 0x1001eed
  fromEnum GDK_KEY_Uhorntilde = 0x1001eee
  fromEnum GDK_KEY_uhorntilde = 0x1001eef
  fromEnum GDK_KEY_Uhornbelowdot = 0x1001ef0
  fromEnum GDK_KEY_uhornbelowdot = 0x1001ef1
  fromEnum GDK_KEY_Ybelowdot = 0x1001ef4
  fromEnum GDK_KEY_ybelowdot = 0x1001ef5
  fromEnum GDK_KEY_Yhook = 0x1001ef6
  fromEnum GDK_KEY_yhook = 0x1001ef7
  fromEnum GDK_KEY_Ytilde = 0x1001ef8
  fromEnum GDK_KEY_ytilde = 0x1001ef9
  fromEnum GDK_KEY_Ohorn = 0x10001a0
  fromEnum GDK_KEY_ohorn = 0x10001a1
  fromEnum GDK_KEY_Uhorn = 0x10001af
  fromEnum GDK_KEY_uhorn = 0x10001b0
  fromEnum GDK_KEY_EcuSign = 0x10020a0
  fromEnum GDK_KEY_ColonSign = 0x10020a1
  fromEnum GDK_KEY_CruzeiroSign = 0x10020a2
  fromEnum GDK_KEY_FFrancSign = 0x10020a3
  fromEnum GDK_KEY_LiraSign = 0x10020a4
  fromEnum GDK_KEY_MillSign = 0x10020a5
  fromEnum GDK_KEY_NairaSign = 0x10020a6
  fromEnum GDK_KEY_PesetaSign = 0x10020a7
  fromEnum GDK_KEY_RupeeSign = 0x10020a8
  fromEnum GDK_KEY_WonSign = 0x10020a9
  fromEnum GDK_KEY_NewSheqelSign = 0x10020aa
  fromEnum GDK_KEY_DongSign = 0x10020ab
  fromEnum GDK_KEY_EuroSign = 0x20ac
  fromEnum GDK_KEY_zerosuperior = 0x1002070
  fromEnum GDK_KEY_foursuperior = 0x1002074
  fromEnum GDK_KEY_fivesuperior = 0x1002075
  fromEnum GDK_KEY_sixsuperior = 0x1002076
  fromEnum GDK_KEY_sevensuperior = 0x1002077
  fromEnum GDK_KEY_eightsuperior = 0x1002078
  fromEnum GDK_KEY_ninesuperior = 0x1002079
  fromEnum GDK_KEY_zerosubscript = 0x1002080
  fromEnum GDK_KEY_onesubscript = 0x1002081
  fromEnum GDK_KEY_twosubscript = 0x1002082
  fromEnum GDK_KEY_threesubscript = 0x1002083
  fromEnum GDK_KEY_foursubscript = 0x1002084
  fromEnum GDK_KEY_fivesubscript = 0x1002085
  fromEnum GDK_KEY_sixsubscript = 0x1002086
  fromEnum GDK_KEY_sevensubscript = 0x1002087
  fromEnum GDK_KEY_eightsubscript = 0x1002088
  fromEnum GDK_KEY_ninesubscript = 0x1002089
  fromEnum GDK_KEY_partdifferential = 0x1002202
  fromEnum GDK_KEY_emptyset = 0x1002205
  fromEnum GDK_KEY_elementof = 0x1002208
  fromEnum GDK_KEY_notelementof = 0x1002209
  fromEnum GDK_KEY_containsas = 0x100220b
  fromEnum GDK_KEY_squareroot = 0x100221a
  fromEnum GDK_KEY_cuberoot = 0x100221b
  fromEnum GDK_KEY_fourthroot = 0x100221c
  fromEnum GDK_KEY_dintegral = 0x100222c
  fromEnum GDK_KEY_tintegral = 0x100222d
  fromEnum GDK_KEY_because = 0x1002235
  fromEnum GDK_KEY_approxeq = 0x1002248
  fromEnum GDK_KEY_notapproxeq = 0x1002247
  fromEnum GDK_KEY_notidentical = 0x1002262
  fromEnum GDK_KEY_stricteq = 0x1002263
  fromEnum GDK_KEY_braille_dot_1 = 0xfff1
  fromEnum GDK_KEY_braille_dot_2 = 0xfff2
  fromEnum GDK_KEY_braille_dot_3 = 0xfff3
  fromEnum GDK_KEY_braille_dot_4 = 0xfff4
  fromEnum GDK_KEY_braille_dot_5 = 0xfff5
  fromEnum GDK_KEY_braille_dot_6 = 0xfff6
  fromEnum GDK_KEY_braille_dot_7 = 0xfff7
  fromEnum GDK_KEY_braille_dot_8 = 0xfff8
  fromEnum GDK_KEY_braille_dot_9 = 0xfff9
  fromEnum GDK_KEY_braille_dot_10 = 0xfffa
  fromEnum GDK_KEY_braille_blank = 0x1002800
  fromEnum GDK_KEY_braille_dots_1 = 0x1002801
  fromEnum GDK_KEY_braille_dots_2 = 0x1002802
  fromEnum GDK_KEY_braille_dots_12 = 0x1002803
  fromEnum GDK_KEY_braille_dots_3 = 0x1002804
  fromEnum GDK_KEY_braille_dots_13 = 0x1002805
  fromEnum GDK_KEY_braille_dots_23 = 0x1002806
  fromEnum GDK_KEY_braille_dots_123 = 0x1002807
  fromEnum GDK_KEY_braille_dots_4 = 0x1002808
  fromEnum GDK_KEY_braille_dots_14 = 0x1002809
  fromEnum GDK_KEY_braille_dots_24 = 0x100280a
  fromEnum GDK_KEY_braille_dots_124 = 0x100280b
  fromEnum GDK_KEY_braille_dots_34 = 0x100280c
  fromEnum GDK_KEY_braille_dots_134 = 0x100280d
  fromEnum GDK_KEY_braille_dots_234 = 0x100280e
  fromEnum GDK_KEY_braille_dots_1234 = 0x100280f
  fromEnum GDK_KEY_braille_dots_5 = 0x1002810
  fromEnum GDK_KEY_braille_dots_15 = 0x1002811
  fromEnum GDK_KEY_braille_dots_25 = 0x1002812
  fromEnum GDK_KEY_braille_dots_125 = 0x1002813
  fromEnum GDK_KEY_braille_dots_35 = 0x1002814
  fromEnum GDK_KEY_braille_dots_135 = 0x1002815
  fromEnum GDK_KEY_braille_dots_235 = 0x1002816
  fromEnum GDK_KEY_braille_dots_1235 = 0x1002817
  fromEnum GDK_KEY_braille_dots_45 = 0x1002818
  fromEnum GDK_KEY_braille_dots_145 = 0x1002819
  fromEnum GDK_KEY_braille_dots_245 = 0x100281a
  fromEnum GDK_KEY_braille_dots_1245 = 0x100281b
  fromEnum GDK_KEY_braille_dots_345 = 0x100281c
  fromEnum GDK_KEY_braille_dots_1345 = 0x100281d
  fromEnum GDK_KEY_braille_dots_2345 = 0x100281e
  fromEnum GDK_KEY_braille_dots_12345 = 0x100281f
  fromEnum GDK_KEY_braille_dots_6 = 0x1002820
  fromEnum GDK_KEY_braille_dots_16 = 0x1002821
  fromEnum GDK_KEY_braille_dots_26 = 0x1002822
  fromEnum GDK_KEY_braille_dots_126 = 0x1002823
  fromEnum GDK_KEY_braille_dots_36 = 0x1002824
  fromEnum GDK_KEY_braille_dots_136 = 0x1002825
  fromEnum GDK_KEY_braille_dots_236 = 0x1002826
  fromEnum GDK_KEY_braille_dots_1236 = 0x1002827
  fromEnum GDK_KEY_braille_dots_46 = 0x1002828
  fromEnum GDK_KEY_braille_dots_146 = 0x1002829
  fromEnum GDK_KEY_braille_dots_246 = 0x100282a
  fromEnum GDK_KEY_braille_dots_1246 = 0x100282b
  fromEnum GDK_KEY_braille_dots_346 = 0x100282c
  fromEnum GDK_KEY_braille_dots_1346 = 0x100282d
  fromEnum GDK_KEY_braille_dots_2346 = 0x100282e
  fromEnum GDK_KEY_braille_dots_12346 = 0x100282f
  fromEnum GDK_KEY_braille_dots_56 = 0x1002830
  fromEnum GDK_KEY_braille_dots_156 = 0x1002831
  fromEnum GDK_KEY_braille_dots_256 = 0x1002832
  fromEnum GDK_KEY_braille_dots_1256 = 0x1002833
  fromEnum GDK_KEY_braille_dots_356 = 0x1002834
  fromEnum GDK_KEY_braille_dots_1356 = 0x1002835
  fromEnum GDK_KEY_braille_dots_2356 = 0x1002836
  fromEnum GDK_KEY_braille_dots_12356 = 0x1002837
  fromEnum GDK_KEY_braille_dots_456 = 0x1002838
  fromEnum GDK_KEY_braille_dots_1456 = 0x1002839
  fromEnum GDK_KEY_braille_dots_2456 = 0x100283a
  fromEnum GDK_KEY_braille_dots_12456 = 0x100283b
  fromEnum GDK_KEY_braille_dots_3456 = 0x100283c
  fromEnum GDK_KEY_braille_dots_13456 = 0x100283d
  fromEnum GDK_KEY_braille_dots_23456 = 0x100283e
  fromEnum GDK_KEY_braille_dots_123456 = 0x100283f
  fromEnum GDK_KEY_braille_dots_7 = 0x1002840
  fromEnum GDK_KEY_braille_dots_17 = 0x1002841
  fromEnum GDK_KEY_braille_dots_27 = 0x1002842
  fromEnum GDK_KEY_braille_dots_127 = 0x1002843
  fromEnum GDK_KEY_braille_dots_37 = 0x1002844
  fromEnum GDK_KEY_braille_dots_137 = 0x1002845
  fromEnum GDK_KEY_braille_dots_237 = 0x1002846
  fromEnum GDK_KEY_braille_dots_1237 = 0x1002847
  fromEnum GDK_KEY_braille_dots_47 = 0x1002848
  fromEnum GDK_KEY_braille_dots_147 = 0x1002849
  fromEnum GDK_KEY_braille_dots_247 = 0x100284a
  fromEnum GDK_KEY_braille_dots_1247 = 0x100284b
  fromEnum GDK_KEY_braille_dots_347 = 0x100284c
  fromEnum GDK_KEY_braille_dots_1347 = 0x100284d
  fromEnum GDK_KEY_braille_dots_2347 = 0x100284e
  fromEnum GDK_KEY_braille_dots_12347 = 0x100284f
  fromEnum GDK_KEY_braille_dots_57 = 0x1002850
  fromEnum GDK_KEY_braille_dots_157 = 0x1002851
  fromEnum GDK_KEY_braille_dots_257 = 0x1002852
  fromEnum GDK_KEY_braille_dots_1257 = 0x1002853
  fromEnum GDK_KEY_braille_dots_357 = 0x1002854
  fromEnum GDK_KEY_braille_dots_1357 = 0x1002855
  fromEnum GDK_KEY_braille_dots_2357 = 0x1002856
  fromEnum GDK_KEY_braille_dots_12357 = 0x1002857
  fromEnum GDK_KEY_braille_dots_457 = 0x1002858
  fromEnum GDK_KEY_braille_dots_1457 = 0x1002859
  fromEnum GDK_KEY_braille_dots_2457 = 0x100285a
  fromEnum GDK_KEY_braille_dots_12457 = 0x100285b
  fromEnum GDK_KEY_braille_dots_3457 = 0x100285c
  fromEnum GDK_KEY_braille_dots_13457 = 0x100285d
  fromEnum GDK_KEY_braille_dots_23457 = 0x100285e
  fromEnum GDK_KEY_braille_dots_123457 = 0x100285f
  fromEnum GDK_KEY_braille_dots_67 = 0x1002860
  fromEnum GDK_KEY_braille_dots_167 = 0x1002861
  fromEnum GDK_KEY_braille_dots_267 = 0x1002862
  fromEnum GDK_KEY_braille_dots_1267 = 0x1002863
  fromEnum GDK_KEY_braille_dots_367 = 0x1002864
  fromEnum GDK_KEY_braille_dots_1367 = 0x1002865
  fromEnum GDK_KEY_braille_dots_2367 = 0x1002866
  fromEnum GDK_KEY_braille_dots_12367 = 0x1002867
  fromEnum GDK_KEY_braille_dots_467 = 0x1002868
  fromEnum GDK_KEY_braille_dots_1467 = 0x1002869
  fromEnum GDK_KEY_braille_dots_2467 = 0x100286a
  fromEnum GDK_KEY_braille_dots_12467 = 0x100286b
  fromEnum GDK_KEY_braille_dots_3467 = 0x100286c
  fromEnum GDK_KEY_braille_dots_13467 = 0x100286d
  fromEnum GDK_KEY_braille_dots_23467 = 0x100286e
  fromEnum GDK_KEY_braille_dots_123467 = 0x100286f
  fromEnum GDK_KEY_braille_dots_567 = 0x1002870
  fromEnum GDK_KEY_braille_dots_1567 = 0x1002871
  fromEnum GDK_KEY_braille_dots_2567 = 0x1002872
  fromEnum GDK_KEY_braille_dots_12567 = 0x1002873
  fromEnum GDK_KEY_braille_dots_3567 = 0x1002874
  fromEnum GDK_KEY_braille_dots_13567 = 0x1002875
  fromEnum GDK_KEY_braille_dots_23567 = 0x1002876
  fromEnum GDK_KEY_braille_dots_123567 = 0x1002877
  fromEnum GDK_KEY_braille_dots_4567 = 0x1002878
  fromEnum GDK_KEY_braille_dots_14567 = 0x1002879
  fromEnum GDK_KEY_braille_dots_24567 = 0x100287a
  fromEnum GDK_KEY_braille_dots_124567 = 0x100287b
  fromEnum GDK_KEY_braille_dots_34567 = 0x100287c
  fromEnum GDK_KEY_braille_dots_134567 = 0x100287d
  fromEnum GDK_KEY_braille_dots_234567 = 0x100287e
  fromEnum GDK_KEY_braille_dots_1234567 = 0x100287f
  fromEnum GDK_KEY_braille_dots_8 = 0x1002880
  fromEnum GDK_KEY_braille_dots_18 = 0x1002881
  fromEnum GDK_KEY_braille_dots_28 = 0x1002882
  fromEnum GDK_KEY_braille_dots_128 = 0x1002883
  fromEnum GDK_KEY_braille_dots_38 = 0x1002884
  fromEnum GDK_KEY_braille_dots_138 = 0x1002885
  fromEnum GDK_KEY_braille_dots_238 = 0x1002886
  fromEnum GDK_KEY_braille_dots_1238 = 0x1002887
  fromEnum GDK_KEY_braille_dots_48 = 0x1002888
  fromEnum GDK_KEY_braille_dots_148 = 0x1002889
  fromEnum GDK_KEY_braille_dots_248 = 0x100288a
  fromEnum GDK_KEY_braille_dots_1248 = 0x100288b
  fromEnum GDK_KEY_braille_dots_348 = 0x100288c
  fromEnum GDK_KEY_braille_dots_1348 = 0x100288d
  fromEnum GDK_KEY_braille_dots_2348 = 0x100288e
  fromEnum GDK_KEY_braille_dots_12348 = 0x100288f
  fromEnum GDK_KEY_braille_dots_58 = 0x1002890
  fromEnum GDK_KEY_braille_dots_158 = 0x1002891
  fromEnum GDK_KEY_braille_dots_258 = 0x1002892
  fromEnum GDK_KEY_braille_dots_1258 = 0x1002893
  fromEnum GDK_KEY_braille_dots_358 = 0x1002894
  fromEnum GDK_KEY_braille_dots_1358 = 0x1002895
  fromEnum GDK_KEY_braille_dots_2358 = 0x1002896
  fromEnum GDK_KEY_braille_dots_12358 = 0x1002897
  fromEnum GDK_KEY_braille_dots_458 = 0x1002898
  fromEnum GDK_KEY_braille_dots_1458 = 0x1002899
  fromEnum GDK_KEY_braille_dots_2458 = 0x100289a
  fromEnum GDK_KEY_braille_dots_12458 = 0x100289b
  fromEnum GDK_KEY_braille_dots_3458 = 0x100289c
  fromEnum GDK_KEY_braille_dots_13458 = 0x100289d
  fromEnum GDK_KEY_braille_dots_23458 = 0x100289e
  fromEnum GDK_KEY_braille_dots_123458 = 0x100289f
  fromEnum GDK_KEY_braille_dots_68 = 0x10028a0
  fromEnum GDK_KEY_braille_dots_168 = 0x10028a1
  fromEnum GDK_KEY_braille_dots_268 = 0x10028a2
  fromEnum GDK_KEY_braille_dots_1268 = 0x10028a3
  fromEnum GDK_KEY_braille_dots_368 = 0x10028a4
  fromEnum GDK_KEY_braille_dots_1368 = 0x10028a5
  fromEnum GDK_KEY_braille_dots_2368 = 0x10028a6
  fromEnum GDK_KEY_braille_dots_12368 = 0x10028a7
  fromEnum GDK_KEY_braille_dots_468 = 0x10028a8
  fromEnum GDK_KEY_braille_dots_1468 = 0x10028a9
  fromEnum GDK_KEY_braille_dots_2468 = 0x10028aa
  fromEnum GDK_KEY_braille_dots_12468 = 0x10028ab
  fromEnum GDK_KEY_braille_dots_3468 = 0x10028ac
  fromEnum GDK_KEY_braille_dots_13468 = 0x10028ad
  fromEnum GDK_KEY_braille_dots_23468 = 0x10028ae
  fromEnum GDK_KEY_braille_dots_123468 = 0x10028af
  fromEnum GDK_KEY_braille_dots_568 = 0x10028b0
  fromEnum GDK_KEY_braille_dots_1568 = 0x10028b1
  fromEnum GDK_KEY_braille_dots_2568 = 0x10028b2
  fromEnum GDK_KEY_braille_dots_12568 = 0x10028b3
  fromEnum GDK_KEY_braille_dots_3568 = 0x10028b4
  fromEnum GDK_KEY_braille_dots_13568 = 0x10028b5
  fromEnum GDK_KEY_braille_dots_23568 = 0x10028b6
  fromEnum GDK_KEY_braille_dots_123568 = 0x10028b7
  fromEnum GDK_KEY_braille_dots_4568 = 0x10028b8
  fromEnum GDK_KEY_braille_dots_14568 = 0x10028b9
  fromEnum GDK_KEY_braille_dots_24568 = 0x10028ba
  fromEnum GDK_KEY_braille_dots_124568 = 0x10028bb
  fromEnum GDK_KEY_braille_dots_34568 = 0x10028bc
  fromEnum GDK_KEY_braille_dots_134568 = 0x10028bd
  fromEnum GDK_KEY_braille_dots_234568 = 0x10028be
  fromEnum GDK_KEY_braille_dots_1234568 = 0x10028bf
  fromEnum GDK_KEY_braille_dots_78 = 0x10028c0
  fromEnum GDK_KEY_braille_dots_178 = 0x10028c1
  fromEnum GDK_KEY_braille_dots_278 = 0x10028c2
  fromEnum GDK_KEY_braille_dots_1278 = 0x10028c3
  fromEnum GDK_KEY_braille_dots_378 = 0x10028c4
  fromEnum GDK_KEY_braille_dots_1378 = 0x10028c5
  fromEnum GDK_KEY_braille_dots_2378 = 0x10028c6
  fromEnum GDK_KEY_braille_dots_12378 = 0x10028c7
  fromEnum GDK_KEY_braille_dots_478 = 0x10028c8
  fromEnum GDK_KEY_braille_dots_1478 = 0x10028c9
  fromEnum GDK_KEY_braille_dots_2478 = 0x10028ca
  fromEnum GDK_KEY_braille_dots_12478 = 0x10028cb
  fromEnum GDK_KEY_braille_dots_3478 = 0x10028cc
  fromEnum GDK_KEY_braille_dots_13478 = 0x10028cd
  fromEnum GDK_KEY_braille_dots_23478 = 0x10028ce
  fromEnum GDK_KEY_braille_dots_123478 = 0x10028cf
  fromEnum GDK_KEY_braille_dots_578 = 0x10028d0
  fromEnum GDK_KEY_braille_dots_1578 = 0x10028d1
  fromEnum GDK_KEY_braille_dots_2578 = 0x10028d2
  fromEnum GDK_KEY_braille_dots_12578 = 0x10028d3
  fromEnum GDK_KEY_braille_dots_3578 = 0x10028d4
  fromEnum GDK_KEY_braille_dots_13578 = 0x10028d5
  fromEnum GDK_KEY_braille_dots_23578 = 0x10028d6
  fromEnum GDK_KEY_braille_dots_123578 = 0x10028d7
  fromEnum GDK_KEY_braille_dots_4578 = 0x10028d8
  fromEnum GDK_KEY_braille_dots_14578 = 0x10028d9
  fromEnum GDK_KEY_braille_dots_24578 = 0x10028da
  fromEnum GDK_KEY_braille_dots_124578 = 0x10028db
  fromEnum GDK_KEY_braille_dots_34578 = 0x10028dc
  fromEnum GDK_KEY_braille_dots_134578 = 0x10028dd
  fromEnum GDK_KEY_braille_dots_234578 = 0x10028de
  fromEnum GDK_KEY_braille_dots_1234578 = 0x10028df
  fromEnum GDK_KEY_braille_dots_678 = 0x10028e0
  fromEnum GDK_KEY_braille_dots_1678 = 0x10028e1
  fromEnum GDK_KEY_braille_dots_2678 = 0x10028e2
  fromEnum GDK_KEY_braille_dots_12678 = 0x10028e3
  fromEnum GDK_KEY_braille_dots_3678 = 0x10028e4
  fromEnum GDK_KEY_braille_dots_13678 = 0x10028e5
  fromEnum GDK_KEY_braille_dots_23678 = 0x10028e6
  fromEnum GDK_KEY_braille_dots_123678 = 0x10028e7
  fromEnum GDK_KEY_braille_dots_4678 = 0x10028e8
  fromEnum GDK_KEY_braille_dots_14678 = 0x10028e9
  fromEnum GDK_KEY_braille_dots_24678 = 0x10028ea
  fromEnum GDK_KEY_braille_dots_124678 = 0x10028eb
  fromEnum GDK_KEY_braille_dots_34678 = 0x10028ec
  fromEnum GDK_KEY_braille_dots_134678 = 0x10028ed
  fromEnum GDK_KEY_braille_dots_234678 = 0x10028ee
  fromEnum GDK_KEY_braille_dots_1234678 = 0x10028ef
  fromEnum GDK_KEY_braille_dots_5678 = 0x10028f0
  fromEnum GDK_KEY_braille_dots_15678 = 0x10028f1
  fromEnum GDK_KEY_braille_dots_25678 = 0x10028f2
  fromEnum GDK_KEY_braille_dots_125678 = 0x10028f3
  fromEnum GDK_KEY_braille_dots_35678 = 0x10028f4
  fromEnum GDK_KEY_braille_dots_135678 = 0x10028f5
  fromEnum GDK_KEY_braille_dots_235678 = 0x10028f6
  fromEnum GDK_KEY_braille_dots_1235678 = 0x10028f7
  fromEnum GDK_KEY_braille_dots_45678 = 0x10028f8
  fromEnum GDK_KEY_braille_dots_145678 = 0x10028f9
  fromEnum GDK_KEY_braille_dots_245678 = 0x10028fa
  fromEnum GDK_KEY_braille_dots_1245678 = 0x10028fb
  fromEnum GDK_KEY_braille_dots_345678 = 0x10028fc
  fromEnum GDK_KEY_braille_dots_1345678 = 0x10028fd
  fromEnum GDK_KEY_braille_dots_2345678 = 0x10028fe
  fromEnum GDK_KEY_braille_dots_12345678 = 0x10028ff
  fromEnum GDK_KEY_Sinh_ng = 0x1000d82
  fromEnum GDK_KEY_Sinh_h2 = 0x1000d83
  fromEnum GDK_KEY_Sinh_a = 0x1000d85
  fromEnum GDK_KEY_Sinh_aa = 0x1000d86
  fromEnum GDK_KEY_Sinh_ae = 0x1000d87
  fromEnum GDK_KEY_Sinh_aee = 0x1000d88
  fromEnum GDK_KEY_Sinh_i = 0x1000d89
  fromEnum GDK_KEY_Sinh_ii = 0x1000d8a
  fromEnum GDK_KEY_Sinh_u = 0x1000d8b
  fromEnum GDK_KEY_Sinh_uu = 0x1000d8c
  fromEnum GDK_KEY_Sinh_ri = 0x1000d8d
  fromEnum GDK_KEY_Sinh_rii = 0x1000d8e
  fromEnum GDK_KEY_Sinh_lu = 0x1000d8f
  fromEnum GDK_KEY_Sinh_luu = 0x1000d90
  fromEnum GDK_KEY_Sinh_e = 0x1000d91
  fromEnum GDK_KEY_Sinh_ee = 0x1000d92
  fromEnum GDK_KEY_Sinh_ai = 0x1000d93
  fromEnum GDK_KEY_Sinh_o = 0x1000d94
  fromEnum GDK_KEY_Sinh_oo = 0x1000d95
  fromEnum GDK_KEY_Sinh_au = 0x1000d96
  fromEnum GDK_KEY_Sinh_ka = 0x1000d9a
  fromEnum GDK_KEY_Sinh_kha = 0x1000d9b
  fromEnum GDK_KEY_Sinh_ga = 0x1000d9c
  fromEnum GDK_KEY_Sinh_gha = 0x1000d9d
  fromEnum GDK_KEY_Sinh_ng2 = 0x1000d9e
  fromEnum GDK_KEY_Sinh_nga = 0x1000d9f
  fromEnum GDK_KEY_Sinh_ca = 0x1000da0
  fromEnum GDK_KEY_Sinh_cha = 0x1000da1
  fromEnum GDK_KEY_Sinh_ja = 0x1000da2
  fromEnum GDK_KEY_Sinh_jha = 0x1000da3
  fromEnum GDK_KEY_Sinh_nya = 0x1000da4
  fromEnum GDK_KEY_Sinh_jnya = 0x1000da5
  fromEnum GDK_KEY_Sinh_nja = 0x1000da6
  fromEnum GDK_KEY_Sinh_tta = 0x1000da7
  fromEnum GDK_KEY_Sinh_ttha = 0x1000da8
  fromEnum GDK_KEY_Sinh_dda = 0x1000da9
  fromEnum GDK_KEY_Sinh_ddha = 0x1000daa
  fromEnum GDK_KEY_Sinh_nna = 0x1000dab
  fromEnum GDK_KEY_Sinh_ndda = 0x1000dac
  fromEnum GDK_KEY_Sinh_tha = 0x1000dad
  fromEnum GDK_KEY_Sinh_thha = 0x1000dae
  fromEnum GDK_KEY_Sinh_dha = 0x1000daf
  fromEnum GDK_KEY_Sinh_dhha = 0x1000db0
  fromEnum GDK_KEY_Sinh_na = 0x1000db1
  fromEnum GDK_KEY_Sinh_ndha = 0x1000db3
  fromEnum GDK_KEY_Sinh_pa = 0x1000db4
  fromEnum GDK_KEY_Sinh_pha = 0x1000db5
  fromEnum GDK_KEY_Sinh_ba = 0x1000db6
  fromEnum GDK_KEY_Sinh_bha = 0x1000db7
  fromEnum GDK_KEY_Sinh_ma = 0x1000db8
  fromEnum GDK_KEY_Sinh_mba = 0x1000db9
  fromEnum GDK_KEY_Sinh_ya = 0x1000dba
  fromEnum GDK_KEY_Sinh_ra = 0x1000dbb
  fromEnum GDK_KEY_Sinh_la = 0x1000dbd
  fromEnum GDK_KEY_Sinh_va = 0x1000dc0
  fromEnum GDK_KEY_Sinh_sha = 0x1000dc1
  fromEnum GDK_KEY_Sinh_ssha = 0x1000dc2
  fromEnum GDK_KEY_Sinh_sa = 0x1000dc3
  fromEnum GDK_KEY_Sinh_ha = 0x1000dc4
  fromEnum GDK_KEY_Sinh_lla = 0x1000dc5
  fromEnum GDK_KEY_Sinh_fa = 0x1000dc6
  fromEnum GDK_KEY_Sinh_al = 0x1000dca
  fromEnum GDK_KEY_Sinh_aa2 = 0x1000dcf
  fromEnum GDK_KEY_Sinh_ae2 = 0x1000dd0
  fromEnum GDK_KEY_Sinh_aee2 = 0x1000dd1
  fromEnum GDK_KEY_Sinh_i2 = 0x1000dd2
  fromEnum GDK_KEY_Sinh_ii2 = 0x1000dd3
  fromEnum GDK_KEY_Sinh_u2 = 0x1000dd4
  fromEnum GDK_KEY_Sinh_uu2 = 0x1000dd6
  fromEnum GDK_KEY_Sinh_ru2 = 0x1000dd8
  fromEnum GDK_KEY_Sinh_e2 = 0x1000dd9
  fromEnum GDK_KEY_Sinh_ee2 = 0x1000dda
  fromEnum GDK_KEY_Sinh_ai2 = 0x1000ddb
  fromEnum GDK_KEY_Sinh_o2 = 0x1000ddc
  fromEnum GDK_KEY_Sinh_oo2 = 0x1000ddd
  fromEnum GDK_KEY_Sinh_au2 = 0x1000dde
  fromEnum GDK_KEY_Sinh_lu2 = 0x1000ddf
  fromEnum GDK_KEY_Sinh_ruu2 = 0x1000df2
  fromEnum GDK_KEY_Sinh_luu2 = 0x1000df3
  fromEnum GDK_KEY_Sinh_kunddaliya = 0x1000df4
  fromEnum GDK_KEY_ModeLock = 0x1008ff01
  fromEnum GDK_KEY_MonBrightnessUp = 0x1008ff02
  fromEnum GDK_KEY_MonBrightnessDown = 0x1008ff03
  fromEnum GDK_KEY_KbdLightOnOff = 0x1008ff04
  fromEnum GDK_KEY_KbdBrightnessUp = 0x1008ff05
  fromEnum GDK_KEY_KbdBrightnessDown = 0x1008ff06
  fromEnum GDK_KEY_Standby = 0x1008ff10
  fromEnum GDK_KEY_AudioLowerVolume = 0x1008ff11
  fromEnum GDK_KEY_AudioMute = 0x1008ff12
  fromEnum GDK_KEY_AudioRaiseVolume = 0x1008ff13
  fromEnum GDK_KEY_AudioPlay = 0x1008ff14
  fromEnum GDK_KEY_AudioStop = 0x1008ff15
  fromEnum GDK_KEY_AudioPrev = 0x1008ff16
  fromEnum GDK_KEY_AudioNext = 0x1008ff17
  fromEnum GDK_KEY_HomePage = 0x1008ff18
  fromEnum GDK_KEY_Mail = 0x1008ff19
  fromEnum GDK_KEY_Start = 0x1008ff1a
  fromEnum GDK_KEY_Search = 0x1008ff1b
  fromEnum GDK_KEY_AudioRecord = 0x1008ff1c
  fromEnum GDK_KEY_Calculator = 0x1008ff1d
  fromEnum GDK_KEY_Memo = 0x1008ff1e
  fromEnum GDK_KEY_ToDoList = 0x1008ff1f
  fromEnum GDK_KEY_Calendar = 0x1008ff20
  fromEnum GDK_KEY_PowerDown = 0x1008ff21
  fromEnum GDK_KEY_ContrastAdjust = 0x1008ff22
  fromEnum GDK_KEY_RockerUp = 0x1008ff23
  fromEnum GDK_KEY_RockerDown = 0x1008ff24
  fromEnum GDK_KEY_RockerEnter = 0x1008ff25
  fromEnum GDK_KEY_Back = 0x1008ff26
  fromEnum GDK_KEY_Forward = 0x1008ff27
  fromEnum GDK_KEY_Stop = 0x1008ff28
  fromEnum GDK_KEY_Refresh = 0x1008ff29
  fromEnum GDK_KEY_PowerOff = 0x1008ff2a
  fromEnum GDK_KEY_WakeUp = 0x1008ff2b
  fromEnum GDK_KEY_Eject = 0x1008ff2c
  fromEnum GDK_KEY_ScreenSaver = 0x1008ff2d
  fromEnum GDK_KEY_WWW = 0x1008ff2e
  fromEnum GDK_KEY_Sleep = 0x1008ff2f
  fromEnum GDK_KEY_Favorites = 0x1008ff30
  fromEnum GDK_KEY_AudioPause = 0x1008ff31
  fromEnum GDK_KEY_AudioMedia = 0x1008ff32
  fromEnum GDK_KEY_MyComputer = 0x1008ff33
  fromEnum GDK_KEY_VendorHome = 0x1008ff34
  fromEnum GDK_KEY_LightBulb = 0x1008ff35
  fromEnum GDK_KEY_Shop = 0x1008ff36
  fromEnum GDK_KEY_History = 0x1008ff37
  fromEnum GDK_KEY_OpenURL = 0x1008ff38
  fromEnum GDK_KEY_AddFavorite = 0x1008ff39
  fromEnum GDK_KEY_HotLinks = 0x1008ff3a
  fromEnum GDK_KEY_BrightnessAdjust = 0x1008ff3b
  fromEnum GDK_KEY_Finance = 0x1008ff3c
  fromEnum GDK_KEY_Community = 0x1008ff3d
  fromEnum GDK_KEY_AudioRewind = 0x1008ff3e
  fromEnum GDK_KEY_BackForward = 0x1008ff3f
  fromEnum GDK_KEY_Launch0 = 0x1008ff40
  fromEnum GDK_KEY_Launch1 = 0x1008ff41
  fromEnum GDK_KEY_Launch2 = 0x1008ff42
  fromEnum GDK_KEY_Launch3 = 0x1008ff43
  fromEnum GDK_KEY_Launch4 = 0x1008ff44
  fromEnum GDK_KEY_Launch5 = 0x1008ff45
  fromEnum GDK_KEY_Launch6 = 0x1008ff46
  fromEnum GDK_KEY_Launch7 = 0x1008ff47
  fromEnum GDK_KEY_Launch8 = 0x1008ff48
  fromEnum GDK_KEY_Launch9 = 0x1008ff49
  fromEnum GDK_KEY_LaunchA = 0x1008ff4a
  fromEnum GDK_KEY_LaunchB = 0x1008ff4b
  fromEnum GDK_KEY_LaunchC = 0x1008ff4c
  fromEnum GDK_KEY_LaunchD = 0x1008ff4d
  fromEnum GDK_KEY_LaunchE = 0x1008ff4e
  fromEnum GDK_KEY_LaunchF = 0x1008ff4f
  fromEnum GDK_KEY_ApplicationLeft = 0x1008ff50
  fromEnum GDK_KEY_ApplicationRight = 0x1008ff51
  fromEnum GDK_KEY_Book = 0x1008ff52
  fromEnum GDK_KEY_CD = 0x1008ff53
  fromEnum GDK_KEY_WindowClear = 0x1008ff55
  fromEnum GDK_KEY_Close = 0x1008ff56
  fromEnum GDK_KEY_Copy = 0x1008ff57
  fromEnum GDK_KEY_Cut = 0x1008ff58
  fromEnum GDK_KEY_Display = 0x1008ff59
  fromEnum GDK_KEY_DOS = 0x1008ff5a
  fromEnum GDK_KEY_Documents = 0x1008ff5b
  fromEnum GDK_KEY_Excel = 0x1008ff5c
  fromEnum GDK_KEY_Explorer = 0x1008ff5d
  fromEnum GDK_KEY_Game = 0x1008ff5e
  fromEnum GDK_KEY_Go = 0x1008ff5f
  fromEnum GDK_KEY_iTouch = 0x1008ff60
  fromEnum GDK_KEY_LogOff = 0x1008ff61
  fromEnum GDK_KEY_Market = 0x1008ff62
  fromEnum GDK_KEY_Meeting = 0x1008ff63
  fromEnum GDK_KEY_MenuKB = 0x1008ff65
  fromEnum GDK_KEY_MenuPB = 0x1008ff66
  fromEnum GDK_KEY_MySites = 0x1008ff67
  fromEnum GDK_KEY_New = 0x1008ff68
  fromEnum GDK_KEY_News = 0x1008ff69
  fromEnum GDK_KEY_OfficeHome = 0x1008ff6a
  fromEnum GDK_KEY_Open = 0x1008ff6b
  fromEnum GDK_KEY_Option = 0x1008ff6c
  fromEnum GDK_KEY_Paste = 0x1008ff6d
  fromEnum GDK_KEY_Phone = 0x1008ff6e
  fromEnum GDK_KEY_Reply = 0x1008ff72
  fromEnum GDK_KEY_Reload = 0x1008ff73
  fromEnum GDK_KEY_RotateWindows = 0x1008ff74
  fromEnum GDK_KEY_RotationPB = 0x1008ff75
  fromEnum GDK_KEY_RotationKB = 0x1008ff76
  fromEnum GDK_KEY_Save = 0x1008ff77
  fromEnum GDK_KEY_ScrollUp = 0x1008ff78
  fromEnum GDK_KEY_ScrollDown = 0x1008ff79
  fromEnum GDK_KEY_ScrollClick = 0x1008ff7a
  fromEnum GDK_KEY_Send = 0x1008ff7b
  fromEnum GDK_KEY_Spell = 0x1008ff7c
  fromEnum GDK_KEY_SplitScreen = 0x1008ff7d
  fromEnum GDK_KEY_Support = 0x1008ff7e
  fromEnum GDK_KEY_TaskPane = 0x1008ff7f
  fromEnum GDK_KEY_Terminal = 0x1008ff80
  fromEnum GDK_KEY_Tools = 0x1008ff81
  fromEnum GDK_KEY_Travel = 0x1008ff82
  fromEnum GDK_KEY_UserPB = 0x1008ff84
  fromEnum GDK_KEY_User1KB = 0x1008ff85
  fromEnum GDK_KEY_User2KB = 0x1008ff86
  fromEnum GDK_KEY_Video = 0x1008ff87
  fromEnum GDK_KEY_WheelButton = 0x1008ff88
  fromEnum GDK_KEY_Word = 0x1008ff89
  fromEnum GDK_KEY_Xfer = 0x1008ff8a
  fromEnum GDK_KEY_ZoomIn = 0x1008ff8b
  fromEnum GDK_KEY_ZoomOut = 0x1008ff8c
  fromEnum GDK_KEY_Away = 0x1008ff8d
  fromEnum GDK_KEY_Messenger = 0x1008ff8e
  fromEnum GDK_KEY_WebCam = 0x1008ff8f
  fromEnum GDK_KEY_MailForward = 0x1008ff90
  fromEnum GDK_KEY_Pictures = 0x1008ff91
  fromEnum GDK_KEY_Music = 0x1008ff92
  fromEnum GDK_KEY_Battery = 0x1008ff93
  fromEnum GDK_KEY_Bluetooth = 0x1008ff94
  fromEnum GDK_KEY_WLAN = 0x1008ff95
  fromEnum GDK_KEY_UWB = 0x1008ff96
  fromEnum GDK_KEY_AudioForward = 0x1008ff97
  fromEnum GDK_KEY_AudioRepeat = 0x1008ff98
  fromEnum GDK_KEY_AudioRandomPlay = 0x1008ff99
  fromEnum GDK_KEY_Subtitle = 0x1008ff9a
  fromEnum GDK_KEY_AudioCycleTrack = 0x1008ff9b
  fromEnum GDK_KEY_CycleAngle = 0x1008ff9c
  fromEnum GDK_KEY_FrameBack = 0x1008ff9d
  fromEnum GDK_KEY_FrameForward = 0x1008ff9e
  fromEnum GDK_KEY_Time = 0x1008ff9f
  fromEnum GDK_KEY_SelectButton = 0x1008ffa0
  fromEnum GDK_KEY_View = 0x1008ffa1
  fromEnum GDK_KEY_TopMenu = 0x1008ffa2
  fromEnum GDK_KEY_Red = 0x1008ffa3
  fromEnum GDK_KEY_Green = 0x1008ffa4
  fromEnum GDK_KEY_Yellow = 0x1008ffa5
  fromEnum GDK_KEY_Blue = 0x1008ffa6
  fromEnum GDK_KEY_Suspend = 0x1008ffa7
  fromEnum GDK_KEY_Hibernate = 0x1008ffa8
  fromEnum GDK_KEY_TouchpadToggle = 0x1008ffa9
  fromEnum GDK_KEY_TouchpadOn = 0x1008ffb0
  fromEnum GDK_KEY_TouchpadOff = 0x1008ffb1
  fromEnum GDK_KEY_AudioMicMute = 0x1008ffb2
  fromEnum GDK_KEY_Switch_VT_1 = 0x1008fe01
  fromEnum GDK_KEY_Switch_VT_2 = 0x1008fe02
  fromEnum GDK_KEY_Switch_VT_3 = 0x1008fe03
  fromEnum GDK_KEY_Switch_VT_4 = 0x1008fe04
  fromEnum GDK_KEY_Switch_VT_5 = 0x1008fe05
  fromEnum GDK_KEY_Switch_VT_6 = 0x1008fe06
  fromEnum GDK_KEY_Switch_VT_7 = 0x1008fe07
  fromEnum GDK_KEY_Switch_VT_8 = 0x1008fe08
  fromEnum GDK_KEY_Switch_VT_9 = 0x1008fe09
  fromEnum GDK_KEY_Switch_VT_10 = 0x1008fe0a
  fromEnum GDK_KEY_Switch_VT_11 = 0x1008fe0b
  fromEnum GDK_KEY_Switch_VT_12 = 0x1008fe0c
  fromEnum GDK_KEY_Ungrab = 0x1008fe20
  fromEnum GDK_KEY_ClearGrab = 0x1008fe21
  fromEnum GDK_KEY_Next_VMode = 0x1008fe22
  fromEnum GDK_KEY_Prev_VMode = 0x1008fe23
  fromEnum GDK_KEY_LogWindowTree = 0x1008fe24
  fromEnum GDK_KEY_LogGrabInfo = 0x1008fe25
  toEnum 0x020 = GDK_KEY_space
  toEnum 0x021 = GDK_KEY_exclam
  toEnum 0x022 = GDK_KEY_quotedbl
  toEnum 0x023 = GDK_KEY_numbersign
  toEnum 0x024 = GDK_KEY_dollar
  toEnum 0x025 = GDK_KEY_percent
  toEnum 0x026 = GDK_KEY_ampersand
  toEnum 0x027 = GDK_KEY_apostrophe
  toEnum 0x028 = GDK_KEY_parenleft
  toEnum 0x029 = GDK_KEY_parenright
  toEnum 0x02a = GDK_KEY_asterisk
  toEnum 0x02b = GDK_KEY_plus
  toEnum 0x02c = GDK_KEY_comma
  toEnum 0x02d = GDK_KEY_minus
  toEnum 0x02e = GDK_KEY_period
  toEnum 0x02f = GDK_KEY_slash
  toEnum 0x030 = GDK_KEY_0
  toEnum 0x031 = GDK_KEY_1
  toEnum 0x032 = GDK_KEY_2
  toEnum 0x033 = GDK_KEY_3
  toEnum 0x034 = GDK_KEY_4
  toEnum 0x035 = GDK_KEY_5
  toEnum 0x036 = GDK_KEY_6
  toEnum 0x037 = GDK_KEY_7
  toEnum 0x038 = GDK_KEY_8
  toEnum 0x039 = GDK_KEY_9
  toEnum 0x03a = GDK_KEY_colon
  toEnum 0x03b = GDK_KEY_semicolon
  toEnum 0x03c = GDK_KEY_less
  toEnum 0x03d = GDK_KEY_equal
  toEnum 0x03e = GDK_KEY_greater
  toEnum 0x03f = GDK_KEY_question
  toEnum 0x040 = GDK_KEY_at
  toEnum 0x041 = GDK_KEY_A
  toEnum 0x042 = GDK_KEY_B
  toEnum 0x043 = GDK_KEY_C
  toEnum 0x044 = GDK_KEY_D
  toEnum 0x045 = GDK_KEY_E
  toEnum 0x046 = GDK_KEY_F
  toEnum 0x047 = GDK_KEY_G
  toEnum 0x048 = GDK_KEY_H
  toEnum 0x049 = GDK_KEY_I
  toEnum 0x04a = GDK_KEY_J
  toEnum 0x04b = GDK_KEY_K
  toEnum 0x04c = GDK_KEY_L
  toEnum 0x04d = GDK_KEY_M
  toEnum 0x04e = GDK_KEY_N
  toEnum 0x04f = GDK_KEY_O
  toEnum 0x050 = GDK_KEY_P
  toEnum 0x051 = GDK_KEY_Q
  toEnum 0x052 = GDK_KEY_R
  toEnum 0x053 = GDK_KEY_S
  toEnum 0x054 = GDK_KEY_T
  toEnum 0x055 = GDK_KEY_U
  toEnum 0x056 = GDK_KEY_V
  toEnum 0x057 = GDK_KEY_W
  toEnum 0x058 = GDK_KEY_X
  toEnum 0x059 = GDK_KEY_Y
  toEnum 0x05a = GDK_KEY_Z
  toEnum 0x05b = GDK_KEY_bracketleft
  toEnum 0x05c = GDK_KEY_backslash
  toEnum 0x05d = GDK_KEY_bracketright
  toEnum 0x05e = GDK_KEY_asciicircum
  toEnum 0x05f = GDK_KEY_underscore
  toEnum 0x060 = GDK_KEY_grave
  toEnum 0x061 = GDK_KEY_a
  toEnum 0x062 = GDK_KEY_b
  toEnum 0x063 = GDK_KEY_c
  toEnum 0x064 = GDK_KEY_d
  toEnum 0x065 = GDK_KEY_e
  toEnum 0x066 = GDK_KEY_f
  toEnum 0x067 = GDK_KEY_g
  toEnum 0x068 = GDK_KEY_h
  toEnum 0x069 = GDK_KEY_i
  toEnum 0x06a = GDK_KEY_j
  toEnum 0x06b = GDK_KEY_k
  toEnum 0x06c = GDK_KEY_l
  toEnum 0x06d = GDK_KEY_m
  toEnum 0x06e = GDK_KEY_n
  toEnum 0x06f = GDK_KEY_o
  toEnum 0x070 = GDK_KEY_p
  toEnum 0x071 = GDK_KEY_q
  toEnum 0x072 = GDK_KEY_r
  toEnum 0x073 = GDK_KEY_s
  toEnum 0x074 = GDK_KEY_t
  toEnum 0x075 = GDK_KEY_u
  toEnum 0x076 = GDK_KEY_v
  toEnum 0x077 = GDK_KEY_w
  toEnum 0x078 = GDK_KEY_x
  toEnum 0x079 = GDK_KEY_y
  toEnum 0x07a = GDK_KEY_z
  toEnum 0x07b = GDK_KEY_braceleft
  toEnum 0x07c = GDK_KEY_bar
  toEnum 0x07d = GDK_KEY_braceright
  toEnum 0x07e = GDK_KEY_asciitilde
  toEnum 0x0a0 = GDK_KEY_nobreakspace
  toEnum 0x0a1 = GDK_KEY_exclamdown
  toEnum 0x0a2 = GDK_KEY_cent
  toEnum 0x0a3 = GDK_KEY_sterling
  toEnum 0x0a4 = GDK_KEY_currency
  toEnum 0x0a5 = GDK_KEY_yen
  toEnum 0x0a6 = GDK_KEY_brokenbar
  toEnum 0x0a7 = GDK_KEY_section
  toEnum 0x0a8 = GDK_KEY_diaeresis
  toEnum 0x0a9 = GDK_KEY_copyright
  toEnum 0x0aa = GDK_KEY_ordfeminine
  toEnum 0x0ab = GDK_KEY_guillemotleft
  toEnum 0x0ac = GDK_KEY_notsign
  toEnum 0x0ad = GDK_KEY_hyphen
  toEnum 0x0ae = GDK_KEY_registered
  toEnum 0x0af = GDK_KEY_macron
  toEnum 0x0b0 = GDK_KEY_degree
  toEnum 0x0b1 = GDK_KEY_plusminus
  toEnum 0x0b2 = GDK_KEY_twosuperior
  toEnum 0x0b3 = GDK_KEY_threesuperior
  toEnum 0x0b4 = GDK_KEY_acute
  toEnum 0x0b5 = GDK_KEY_mu
  toEnum 0x0b6 = GDK_KEY_paragraph
  toEnum 0x0b7 = GDK_KEY_periodcentered
  toEnum 0x0b8 = GDK_KEY_cedilla
  toEnum 0x0b9 = GDK_KEY_onesuperior
  toEnum 0x0ba = GDK_KEY_masculine
  toEnum 0x0bb = GDK_KEY_guillemotright
  toEnum 0x0bc = GDK_KEY_onequarter
  toEnum 0x0bd = GDK_KEY_onehalf
  toEnum 0x0be = GDK_KEY_threequarters
  toEnum 0x0bf = GDK_KEY_questiondown
  toEnum 0x0c0 = GDK_KEY_Agrave
  toEnum 0x0c1 = GDK_KEY_Aacute
  toEnum 0x0c2 = GDK_KEY_Acircumflex
  toEnum 0x0c3 = GDK_KEY_Atilde
  toEnum 0x0c4 = GDK_KEY_Adiaeresis
  toEnum 0x0c5 = GDK_KEY_Aring
  toEnum 0x0c6 = GDK_KEY_AE
  toEnum 0x0c7 = GDK_KEY_Ccedilla
  toEnum 0x0c8 = GDK_KEY_Egrave
  toEnum 0x0c9 = GDK_KEY_Eacute
  toEnum 0x0ca = GDK_KEY_Ecircumflex
  toEnum 0x0cb = GDK_KEY_Ediaeresis
  toEnum 0x0cc = GDK_KEY_Igrave
  toEnum 0x0cd = GDK_KEY_Iacute
  toEnum 0x0ce = GDK_KEY_Icircumflex
  toEnum 0x0cf = GDK_KEY_Idiaeresis
  toEnum 0x0d0 = GDK_KEY_Eth
  toEnum 0x0d1 = GDK_KEY_Ntilde
  toEnum 0x0d2 = GDK_KEY_Ograve
  toEnum 0x0d3 = GDK_KEY_Oacute
  toEnum 0x0d4 = GDK_KEY_Ocircumflex
  toEnum 0x0d5 = GDK_KEY_Otilde
  toEnum 0x0d6 = GDK_KEY_Odiaeresis
  toEnum 0x0d7 = GDK_KEY_multiply
  toEnum 0x0d8 = GDK_KEY_Oslash
  toEnum 0x0d9 = GDK_KEY_Ugrave
  toEnum 0x0da = GDK_KEY_Uacute
  toEnum 0x0db = GDK_KEY_Ucircumflex
  toEnum 0x0dc = GDK_KEY_Udiaeresis
  toEnum 0x0dd = GDK_KEY_Yacute
  toEnum 0x0de = GDK_KEY_Thorn
  toEnum 0x0df = GDK_KEY_ssharp
  toEnum 0x0e0 = GDK_KEY_agrave
  toEnum 0x0e1 = GDK_KEY_aacute
  toEnum 0x0e2 = GDK_KEY_acircumflex
  toEnum 0x0e3 = GDK_KEY_atilde
  toEnum 0x0e4 = GDK_KEY_adiaeresis
  toEnum 0x0e5 = GDK_KEY_aring
  toEnum 0x0e6 = GDK_KEY_ae
  toEnum 0x0e7 = GDK_KEY_ccedilla
  toEnum 0x0e8 = GDK_KEY_egrave
  toEnum 0x0e9 = GDK_KEY_eacute
  toEnum 0x0ea = GDK_KEY_ecircumflex
  toEnum 0x0eb = GDK_KEY_ediaeresis
  toEnum 0x0ec = GDK_KEY_igrave
  toEnum 0x0ed = GDK_KEY_iacute
  toEnum 0x0ee = GDK_KEY_icircumflex
  toEnum 0x0ef = GDK_KEY_idiaeresis
  toEnum 0x0f0 = GDK_KEY_eth
  toEnum 0x0f1 = GDK_KEY_ntilde
  toEnum 0x0f2 = GDK_KEY_ograve
  toEnum 0x0f3 = GDK_KEY_oacute
  toEnum 0x0f4 = GDK_KEY_ocircumflex
  toEnum 0x0f5 = GDK_KEY_otilde
  toEnum 0x0f6 = GDK_KEY_odiaeresis
  toEnum 0x0f7 = GDK_KEY_division
  toEnum 0x0f8 = GDK_KEY_oslash
  toEnum 0x0f9 = GDK_KEY_ugrave
  toEnum 0x0fa = GDK_KEY_uacute
  toEnum 0x0fb = GDK_KEY_ucircumflex
  toEnum 0x0fc = GDK_KEY_udiaeresis
  toEnum 0x0fd = GDK_KEY_yacute
  toEnum 0x0fe = GDK_KEY_thorn
  toEnum 0x0ff = GDK_KEY_ydiaeresis
  toEnum 0x100012c = GDK_KEY_Ibreve
  toEnum 0x100012d = GDK_KEY_ibreve
  toEnum 0x1000174 = GDK_KEY_Wcircumflex
  toEnum 0x1000175 = GDK_KEY_wcircumflex
  toEnum 0x1000176 = GDK_KEY_Ycircumflex
  toEnum 0x1000177 = GDK_KEY_ycircumflex
  toEnum 0x100018f = GDK_KEY_SCHWA
  toEnum 0x100019f = GDK_KEY_Obarred
  toEnum 0x10001a0 = GDK_KEY_Ohorn
  toEnum 0x10001a1 = GDK_KEY_ohorn
  toEnum 0x10001af = GDK_KEY_Uhorn
  toEnum 0x10001b0 = GDK_KEY_uhorn
  toEnum 0x10001b5 = GDK_KEY_Zstroke
  toEnum 0x10001b6 = GDK_KEY_zstroke
  toEnum 0x10001b7 = GDK_KEY_EZH
  toEnum 0x10001d1 = GDK_KEY_Ocaron
  toEnum 0x10001d2 = GDK_KEY_ocaron
  toEnum 0x10001e6 = GDK_KEY_Gcaron
  toEnum 0x10001e7 = GDK_KEY_gcaron
  toEnum 0x1000259 = GDK_KEY_schwa
  toEnum 0x1000275 = GDK_KEY_obarred
  toEnum 0x1000292 = GDK_KEY_ezh
  toEnum 0x1000492 = GDK_KEY_Cyrillic_GHE_bar
  toEnum 0x1000493 = GDK_KEY_Cyrillic_ghe_bar
  toEnum 0x1000496 = GDK_KEY_Cyrillic_ZHE_descender
  toEnum 0x1000497 = GDK_KEY_Cyrillic_zhe_descender
  toEnum 0x100049a = GDK_KEY_Cyrillic_KA_descender
  toEnum 0x100049b = GDK_KEY_Cyrillic_ka_descender
  toEnum 0x100049c = GDK_KEY_Cyrillic_KA_vertstroke
  toEnum 0x100049d = GDK_KEY_Cyrillic_ka_vertstroke
  toEnum 0x10004a2 = GDK_KEY_Cyrillic_EN_descender
  toEnum 0x10004a3 = GDK_KEY_Cyrillic_en_descender
  toEnum 0x10004ae = GDK_KEY_Cyrillic_U_straight
  toEnum 0x10004af = GDK_KEY_Cyrillic_u_straight
  toEnum 0x10004b0 = GDK_KEY_Cyrillic_U_straight_bar
  toEnum 0x10004b1 = GDK_KEY_Cyrillic_u_straight_bar
  toEnum 0x10004b2 = GDK_KEY_Cyrillic_HA_descender
  toEnum 0x10004b3 = GDK_KEY_Cyrillic_ha_descender
  toEnum 0x10004b6 = GDK_KEY_Cyrillic_CHE_descender
  toEnum 0x10004b7 = GDK_KEY_Cyrillic_che_descender
  toEnum 0x10004b8 = GDK_KEY_Cyrillic_CHE_vertstroke
  toEnum 0x10004b9 = GDK_KEY_Cyrillic_che_vertstroke
  toEnum 0x10004ba = GDK_KEY_Cyrillic_SHHA
  toEnum 0x10004bb = GDK_KEY_Cyrillic_shha
  toEnum 0x10004d8 = GDK_KEY_Cyrillic_SCHWA
  toEnum 0x10004d9 = GDK_KEY_Cyrillic_schwa
  toEnum 0x10004e2 = GDK_KEY_Cyrillic_I_macron
  toEnum 0x10004e3 = GDK_KEY_Cyrillic_i_macron
  toEnum 0x10004e8 = GDK_KEY_Cyrillic_O_bar
  toEnum 0x10004e9 = GDK_KEY_Cyrillic_o_bar
  toEnum 0x10004ee = GDK_KEY_Cyrillic_U_macron
  toEnum 0x10004ef = GDK_KEY_Cyrillic_u_macron
  toEnum 0x1000531 = GDK_KEY_Armenian_AYB
  toEnum 0x1000532 = GDK_KEY_Armenian_BEN
  toEnum 0x1000533 = GDK_KEY_Armenian_GIM
  toEnum 0x1000534 = GDK_KEY_Armenian_DA
  toEnum 0x1000535 = GDK_KEY_Armenian_YECH
  toEnum 0x1000536 = GDK_KEY_Armenian_ZA
  toEnum 0x1000537 = GDK_KEY_Armenian_E
  toEnum 0x1000538 = GDK_KEY_Armenian_AT
  toEnum 0x1000539 = GDK_KEY_Armenian_TO
  toEnum 0x100053a = GDK_KEY_Armenian_ZHE
  toEnum 0x100053b = GDK_KEY_Armenian_INI
  toEnum 0x100053c = GDK_KEY_Armenian_LYUN
  toEnum 0x100053d = GDK_KEY_Armenian_KHE
  toEnum 0x100053e = GDK_KEY_Armenian_TSA
  toEnum 0x100053f = GDK_KEY_Armenian_KEN
  toEnum 0x1000540 = GDK_KEY_Armenian_HO
  toEnum 0x1000541 = GDK_KEY_Armenian_DZA
  toEnum 0x1000542 = GDK_KEY_Armenian_GHAT
  toEnum 0x1000543 = GDK_KEY_Armenian_TCHE
  toEnum 0x1000544 = GDK_KEY_Armenian_MEN
  toEnum 0x1000545 = GDK_KEY_Armenian_HI
  toEnum 0x1000546 = GDK_KEY_Armenian_NU
  toEnum 0x1000547 = GDK_KEY_Armenian_SHA
  toEnum 0x1000548 = GDK_KEY_Armenian_VO
  toEnum 0x1000549 = GDK_KEY_Armenian_CHA
  toEnum 0x100054a = GDK_KEY_Armenian_PE
  toEnum 0x100054b = GDK_KEY_Armenian_JE
  toEnum 0x100054c = GDK_KEY_Armenian_RA
  toEnum 0x100054d = GDK_KEY_Armenian_SE
  toEnum 0x100054e = GDK_KEY_Armenian_VEV
  toEnum 0x100054f = GDK_KEY_Armenian_TYUN
  toEnum 0x1000550 = GDK_KEY_Armenian_RE
  toEnum 0x1000551 = GDK_KEY_Armenian_TSO
  toEnum 0x1000552 = GDK_KEY_Armenian_VYUN
  toEnum 0x1000553 = GDK_KEY_Armenian_PYUR
  toEnum 0x1000554 = GDK_KEY_Armenian_KE
  toEnum 0x1000555 = GDK_KEY_Armenian_O
  toEnum 0x1000556 = GDK_KEY_Armenian_FE
  toEnum 0x100055a = GDK_KEY_Armenian_apostrophe
  toEnum 0x100055b = GDK_KEY_Armenian_shesht
  toEnum 0x100055c = GDK_KEY_Armenian_amanak
  toEnum 0x100055d = GDK_KEY_Armenian_but
  toEnum 0x100055e = GDK_KEY_Armenian_paruyk
  toEnum 0x1000561 = GDK_KEY_Armenian_ayb
  toEnum 0x1000562 = GDK_KEY_Armenian_ben
  toEnum 0x1000563 = GDK_KEY_Armenian_gim
  toEnum 0x1000564 = GDK_KEY_Armenian_da
  toEnum 0x1000565 = GDK_KEY_Armenian_yech
  toEnum 0x1000566 = GDK_KEY_Armenian_za
  toEnum 0x1000567 = GDK_KEY_Armenian_e
  toEnum 0x1000568 = GDK_KEY_Armenian_at
  toEnum 0x1000569 = GDK_KEY_Armenian_to
  toEnum 0x100056a = GDK_KEY_Armenian_zhe
  toEnum 0x100056b = GDK_KEY_Armenian_ini
  toEnum 0x100056c = GDK_KEY_Armenian_lyun
  toEnum 0x100056d = GDK_KEY_Armenian_khe
  toEnum 0x100056e = GDK_KEY_Armenian_tsa
  toEnum 0x100056f = GDK_KEY_Armenian_ken
  toEnum 0x1000570 = GDK_KEY_Armenian_ho
  toEnum 0x1000571 = GDK_KEY_Armenian_dza
  toEnum 0x1000572 = GDK_KEY_Armenian_ghat
  toEnum 0x1000573 = GDK_KEY_Armenian_tche
  toEnum 0x1000574 = GDK_KEY_Armenian_men
  toEnum 0x1000575 = GDK_KEY_Armenian_hi
  toEnum 0x1000576 = GDK_KEY_Armenian_nu
  toEnum 0x1000577 = GDK_KEY_Armenian_sha
  toEnum 0x1000578 = GDK_KEY_Armenian_vo
  toEnum 0x1000579 = GDK_KEY_Armenian_cha
  toEnum 0x100057a = GDK_KEY_Armenian_pe
  toEnum 0x100057b = GDK_KEY_Armenian_je
  toEnum 0x100057c = GDK_KEY_Armenian_ra
  toEnum 0x100057d = GDK_KEY_Armenian_se
  toEnum 0x100057e = GDK_KEY_Armenian_vev
  toEnum 0x100057f = GDK_KEY_Armenian_tyun
  toEnum 0x1000580 = GDK_KEY_Armenian_re
  toEnum 0x1000581 = GDK_KEY_Armenian_tso
  toEnum 0x1000582 = GDK_KEY_Armenian_vyun
  toEnum 0x1000583 = GDK_KEY_Armenian_pyur
  toEnum 0x1000584 = GDK_KEY_Armenian_ke
  toEnum 0x1000585 = GDK_KEY_Armenian_o
  toEnum 0x1000586 = GDK_KEY_Armenian_fe
  toEnum 0x1000587 = GDK_KEY_Armenian_ligature_ew
  toEnum 0x1000589 = GDK_KEY_Armenian_verjaket
  toEnum 0x100058a = GDK_KEY_Armenian_yentamna
  toEnum 0x1000653 = GDK_KEY_Arabic_madda_above
  toEnum 0x1000654 = GDK_KEY_Arabic_hamza_above
  toEnum 0x1000655 = GDK_KEY_Arabic_hamza_below
  toEnum 0x1000660 = GDK_KEY_Arabic_0
  toEnum 0x1000661 = GDK_KEY_Arabic_1
  toEnum 0x1000662 = GDK_KEY_Arabic_2
  toEnum 0x1000663 = GDK_KEY_Arabic_3
  toEnum 0x1000664 = GDK_KEY_Arabic_4
  toEnum 0x1000665 = GDK_KEY_Arabic_5
  toEnum 0x1000666 = GDK_KEY_Arabic_6
  toEnum 0x1000667 = GDK_KEY_Arabic_7
  toEnum 0x1000668 = GDK_KEY_Arabic_8
  toEnum 0x1000669 = GDK_KEY_Arabic_9
  toEnum 0x100066a = GDK_KEY_Arabic_percent
  toEnum 0x1000670 = GDK_KEY_Arabic_superscript_alef
  toEnum 0x1000679 = GDK_KEY_Arabic_tteh
  toEnum 0x100067e = GDK_KEY_Arabic_peh
  toEnum 0x1000686 = GDK_KEY_Arabic_tcheh
  toEnum 0x1000688 = GDK_KEY_Arabic_ddal
  toEnum 0x1000691 = GDK_KEY_Arabic_rreh
  toEnum 0x1000698 = GDK_KEY_Arabic_jeh
  toEnum 0x10006a4 = GDK_KEY_Arabic_veh
  toEnum 0x10006a9 = GDK_KEY_Arabic_keheh
  toEnum 0x10006af = GDK_KEY_Arabic_gaf
  toEnum 0x10006ba = GDK_KEY_Arabic_noon_ghunna
  toEnum 0x10006be = GDK_KEY_Arabic_heh_doachashmee
  toEnum 0x10006c1 = GDK_KEY_Arabic_heh_goal
  toEnum 0x10006cc = GDK_KEY_Arabic_farsi_yeh
  toEnum 0x10006d2 = GDK_KEY_Arabic_yeh_baree
  toEnum 0x10006d4 = GDK_KEY_Arabic_fullstop
  toEnum 0x10006f0 = GDK_KEY_Farsi_0
  toEnum 0x10006f1 = GDK_KEY_Farsi_1
  toEnum 0x10006f2 = GDK_KEY_Farsi_2
  toEnum 0x10006f3 = GDK_KEY_Farsi_3
  toEnum 0x10006f4 = GDK_KEY_Farsi_4
  toEnum 0x10006f5 = GDK_KEY_Farsi_5
  toEnum 0x10006f6 = GDK_KEY_Farsi_6
  toEnum 0x10006f7 = GDK_KEY_Farsi_7
  toEnum 0x10006f8 = GDK_KEY_Farsi_8
  toEnum 0x10006f9 = GDK_KEY_Farsi_9
  toEnum 0x1000d82 = GDK_KEY_Sinh_ng
  toEnum 0x1000d83 = GDK_KEY_Sinh_h2
  toEnum 0x1000d85 = GDK_KEY_Sinh_a
  toEnum 0x1000d86 = GDK_KEY_Sinh_aa
  toEnum 0x1000d87 = GDK_KEY_Sinh_ae
  toEnum 0x1000d88 = GDK_KEY_Sinh_aee
  toEnum 0x1000d89 = GDK_KEY_Sinh_i
  toEnum 0x1000d8a = GDK_KEY_Sinh_ii
  toEnum 0x1000d8b = GDK_KEY_Sinh_u
  toEnum 0x1000d8c = GDK_KEY_Sinh_uu
  toEnum 0x1000d8d = GDK_KEY_Sinh_ri
  toEnum 0x1000d8e = GDK_KEY_Sinh_rii
  toEnum 0x1000d8f = GDK_KEY_Sinh_lu
  toEnum 0x1000d90 = GDK_KEY_Sinh_luu
  toEnum 0x1000d91 = GDK_KEY_Sinh_e
  toEnum 0x1000d92 = GDK_KEY_Sinh_ee
  toEnum 0x1000d93 = GDK_KEY_Sinh_ai
  toEnum 0x1000d94 = GDK_KEY_Sinh_o
  toEnum 0x1000d95 = GDK_KEY_Sinh_oo
  toEnum 0x1000d96 = GDK_KEY_Sinh_au
  toEnum 0x1000d9a = GDK_KEY_Sinh_ka
  toEnum 0x1000d9b = GDK_KEY_Sinh_kha
  toEnum 0x1000d9c = GDK_KEY_Sinh_ga
  toEnum 0x1000d9d = GDK_KEY_Sinh_gha
  toEnum 0x1000d9e = GDK_KEY_Sinh_ng2
  toEnum 0x1000d9f = GDK_KEY_Sinh_nga
  toEnum 0x1000da0 = GDK_KEY_Sinh_ca
  toEnum 0x1000da1 = GDK_KEY_Sinh_cha
  toEnum 0x1000da2 = GDK_KEY_Sinh_ja
  toEnum 0x1000da3 = GDK_KEY_Sinh_jha
  toEnum 0x1000da4 = GDK_KEY_Sinh_nya
  toEnum 0x1000da5 = GDK_KEY_Sinh_jnya
  toEnum 0x1000da6 = GDK_KEY_Sinh_nja
  toEnum 0x1000da7 = GDK_KEY_Sinh_tta
  toEnum 0x1000da8 = GDK_KEY_Sinh_ttha
  toEnum 0x1000da9 = GDK_KEY_Sinh_dda
  toEnum 0x1000daa = GDK_KEY_Sinh_ddha
  toEnum 0x1000dab = GDK_KEY_Sinh_nna
  toEnum 0x1000dac = GDK_KEY_Sinh_ndda
  toEnum 0x1000dad = GDK_KEY_Sinh_tha
  toEnum 0x1000dae = GDK_KEY_Sinh_thha
  toEnum 0x1000daf = GDK_KEY_Sinh_dha
  toEnum 0x1000db0 = GDK_KEY_Sinh_dhha
  toEnum 0x1000db1 = GDK_KEY_Sinh_na
  toEnum 0x1000db3 = GDK_KEY_Sinh_ndha
  toEnum 0x1000db4 = GDK_KEY_Sinh_pa
  toEnum 0x1000db5 = GDK_KEY_Sinh_pha
  toEnum 0x1000db6 = GDK_KEY_Sinh_ba
  toEnum 0x1000db7 = GDK_KEY_Sinh_bha
  toEnum 0x1000db8 = GDK_KEY_Sinh_ma
  toEnum 0x1000db9 = GDK_KEY_Sinh_mba
  toEnum 0x1000dba = GDK_KEY_Sinh_ya
  toEnum 0x1000dbb = GDK_KEY_Sinh_ra
  toEnum 0x1000dbd = GDK_KEY_Sinh_la
  toEnum 0x1000dc0 = GDK_KEY_Sinh_va
  toEnum 0x1000dc1 = GDK_KEY_Sinh_sha
  toEnum 0x1000dc2 = GDK_KEY_Sinh_ssha
  toEnum 0x1000dc3 = GDK_KEY_Sinh_sa
  toEnum 0x1000dc4 = GDK_KEY_Sinh_ha
  toEnum 0x1000dc5 = GDK_KEY_Sinh_lla
  toEnum 0x1000dc6 = GDK_KEY_Sinh_fa
  toEnum 0x1000dca = GDK_KEY_Sinh_al
  toEnum 0x1000dcf = GDK_KEY_Sinh_aa2
  toEnum 0x1000dd0 = GDK_KEY_Sinh_ae2
  toEnum 0x1000dd1 = GDK_KEY_Sinh_aee2
  toEnum 0x1000dd2 = GDK_KEY_Sinh_i2
  toEnum 0x1000dd3 = GDK_KEY_Sinh_ii2
  toEnum 0x1000dd4 = GDK_KEY_Sinh_u2
  toEnum 0x1000dd6 = GDK_KEY_Sinh_uu2
  toEnum 0x1000dd8 = GDK_KEY_Sinh_ru2
  toEnum 0x1000dd9 = GDK_KEY_Sinh_e2
  toEnum 0x1000dda = GDK_KEY_Sinh_ee2
  toEnum 0x1000ddb = GDK_KEY_Sinh_ai2
  toEnum 0x1000ddc = GDK_KEY_Sinh_o2
  toEnum 0x1000ddd = GDK_KEY_Sinh_oo2
  toEnum 0x1000dde = GDK_KEY_Sinh_au2
  toEnum 0x1000ddf = GDK_KEY_Sinh_lu2
  toEnum 0x1000df2 = GDK_KEY_Sinh_ruu2
  toEnum 0x1000df3 = GDK_KEY_Sinh_luu2
  toEnum 0x1000df4 = GDK_KEY_Sinh_kunddaliya
  toEnum 0x10010d0 = GDK_KEY_Georgian_an
  toEnum 0x10010d1 = GDK_KEY_Georgian_ban
  toEnum 0x10010d2 = GDK_KEY_Georgian_gan
  toEnum 0x10010d3 = GDK_KEY_Georgian_don
  toEnum 0x10010d4 = GDK_KEY_Georgian_en
  toEnum 0x10010d5 = GDK_KEY_Georgian_vin
  toEnum 0x10010d6 = GDK_KEY_Georgian_zen
  toEnum 0x10010d7 = GDK_KEY_Georgian_tan
  toEnum 0x10010d8 = GDK_KEY_Georgian_in
  toEnum 0x10010d9 = GDK_KEY_Georgian_kan
  toEnum 0x10010da = GDK_KEY_Georgian_las
  toEnum 0x10010db = GDK_KEY_Georgian_man
  toEnum 0x10010dc = GDK_KEY_Georgian_nar
  toEnum 0x10010dd = GDK_KEY_Georgian_on
  toEnum 0x10010de = GDK_KEY_Georgian_par
  toEnum 0x10010df = GDK_KEY_Georgian_zhar
  toEnum 0x10010e0 = GDK_KEY_Georgian_rae
  toEnum 0x10010e1 = GDK_KEY_Georgian_san
  toEnum 0x10010e2 = GDK_KEY_Georgian_tar
  toEnum 0x10010e3 = GDK_KEY_Georgian_un
  toEnum 0x10010e4 = GDK_KEY_Georgian_phar
  toEnum 0x10010e5 = GDK_KEY_Georgian_khar
  toEnum 0x10010e6 = GDK_KEY_Georgian_ghan
  toEnum 0x10010e7 = GDK_KEY_Georgian_qar
  toEnum 0x10010e8 = GDK_KEY_Georgian_shin
  toEnum 0x10010e9 = GDK_KEY_Georgian_chin
  toEnum 0x10010ea = GDK_KEY_Georgian_can
  toEnum 0x10010eb = GDK_KEY_Georgian_jil
  toEnum 0x10010ec = GDK_KEY_Georgian_cil
  toEnum 0x10010ed = GDK_KEY_Georgian_char
  toEnum 0x10010ee = GDK_KEY_Georgian_xan
  toEnum 0x10010ef = GDK_KEY_Georgian_jhan
  toEnum 0x10010f0 = GDK_KEY_Georgian_hae
  toEnum 0x10010f1 = GDK_KEY_Georgian_he
  toEnum 0x10010f2 = GDK_KEY_Georgian_hie
  toEnum 0x10010f3 = GDK_KEY_Georgian_we
  toEnum 0x10010f4 = GDK_KEY_Georgian_har
  toEnum 0x10010f5 = GDK_KEY_Georgian_hoe
  toEnum 0x10010f6 = GDK_KEY_Georgian_fi
  toEnum 0x1001e02 = GDK_KEY_Babovedot
  toEnum 0x1001e03 = GDK_KEY_babovedot
  toEnum 0x1001e0a = GDK_KEY_Dabovedot
  toEnum 0x1001e0b = GDK_KEY_dabovedot
  toEnum 0x1001e1e = GDK_KEY_Fabovedot
  toEnum 0x1001e1f = GDK_KEY_fabovedot
  toEnum 0x1001e36 = GDK_KEY_Lbelowdot
  toEnum 0x1001e37 = GDK_KEY_lbelowdot
  toEnum 0x1001e40 = GDK_KEY_Mabovedot
  toEnum 0x1001e41 = GDK_KEY_mabovedot
  toEnum 0x1001e56 = GDK_KEY_Pabovedot
  toEnum 0x1001e57 = GDK_KEY_pabovedot
  toEnum 0x1001e60 = GDK_KEY_Sabovedot
  toEnum 0x1001e61 = GDK_KEY_sabovedot
  toEnum 0x1001e6a = GDK_KEY_Tabovedot
  toEnum 0x1001e6b = GDK_KEY_tabovedot
  toEnum 0x1001e80 = GDK_KEY_Wgrave
  toEnum 0x1001e81 = GDK_KEY_wgrave
  toEnum 0x1001e82 = GDK_KEY_Wacute
  toEnum 0x1001e83 = GDK_KEY_wacute
  toEnum 0x1001e84 = GDK_KEY_Wdiaeresis
  toEnum 0x1001e85 = GDK_KEY_wdiaeresis
  toEnum 0x1001e8a = GDK_KEY_Xabovedot
  toEnum 0x1001e8b = GDK_KEY_xabovedot
  toEnum 0x1001ea0 = GDK_KEY_Abelowdot
  toEnum 0x1001ea1 = GDK_KEY_abelowdot
  toEnum 0x1001ea2 = GDK_KEY_Ahook
  toEnum 0x1001ea3 = GDK_KEY_ahook
  toEnum 0x1001ea4 = GDK_KEY_Acircumflexacute
  toEnum 0x1001ea5 = GDK_KEY_acircumflexacute
  toEnum 0x1001ea6 = GDK_KEY_Acircumflexgrave
  toEnum 0x1001ea7 = GDK_KEY_acircumflexgrave
  toEnum 0x1001ea8 = GDK_KEY_Acircumflexhook
  toEnum 0x1001ea9 = GDK_KEY_acircumflexhook
  toEnum 0x1001eaa = GDK_KEY_Acircumflextilde
  toEnum 0x1001eab = GDK_KEY_acircumflextilde
  toEnum 0x1001eac = GDK_KEY_Acircumflexbelowdot
  toEnum 0x1001ead = GDK_KEY_acircumflexbelowdot
  toEnum 0x1001eae = GDK_KEY_Abreveacute
  toEnum 0x1001eaf = GDK_KEY_abreveacute
  toEnum 0x1001eb0 = GDK_KEY_Abrevegrave
  toEnum 0x1001eb1 = GDK_KEY_abrevegrave
  toEnum 0x1001eb2 = GDK_KEY_Abrevehook
  toEnum 0x1001eb3 = GDK_KEY_abrevehook
  toEnum 0x1001eb4 = GDK_KEY_Abrevetilde
  toEnum 0x1001eb5 = GDK_KEY_abrevetilde
  toEnum 0x1001eb6 = GDK_KEY_Abrevebelowdot
  toEnum 0x1001eb7 = GDK_KEY_abrevebelowdot
  toEnum 0x1001eb8 = GDK_KEY_Ebelowdot
  toEnum 0x1001eb9 = GDK_KEY_ebelowdot
  toEnum 0x1001eba = GDK_KEY_Ehook
  toEnum 0x1001ebb = GDK_KEY_ehook
  toEnum 0x1001ebc = GDK_KEY_Etilde
  toEnum 0x1001ebd = GDK_KEY_etilde
  toEnum 0x1001ebe = GDK_KEY_Ecircumflexacute
  toEnum 0x1001ebf = GDK_KEY_ecircumflexacute
  toEnum 0x1001ec0 = GDK_KEY_Ecircumflexgrave
  toEnum 0x1001ec1 = GDK_KEY_ecircumflexgrave
  toEnum 0x1001ec2 = GDK_KEY_Ecircumflexhook
  toEnum 0x1001ec3 = GDK_KEY_ecircumflexhook
  toEnum 0x1001ec4 = GDK_KEY_Ecircumflextilde
  toEnum 0x1001ec5 = GDK_KEY_ecircumflextilde
  toEnum 0x1001ec6 = GDK_KEY_Ecircumflexbelowdot
  toEnum 0x1001ec7 = GDK_KEY_ecircumflexbelowdot
  toEnum 0x1001ec8 = GDK_KEY_Ihook
  toEnum 0x1001ec9 = GDK_KEY_ihook
  toEnum 0x1001eca = GDK_KEY_Ibelowdot
  toEnum 0x1001ecb = GDK_KEY_ibelowdot
  toEnum 0x1001ecc = GDK_KEY_Obelowdot
  toEnum 0x1001ecd = GDK_KEY_obelowdot
  toEnum 0x1001ece = GDK_KEY_Ohook
  toEnum 0x1001ecf = GDK_KEY_ohook
  toEnum 0x1001ed0 = GDK_KEY_Ocircumflexacute
  toEnum 0x1001ed1 = GDK_KEY_ocircumflexacute
  toEnum 0x1001ed2 = GDK_KEY_Ocircumflexgrave
  toEnum 0x1001ed3 = GDK_KEY_ocircumflexgrave
  toEnum 0x1001ed4 = GDK_KEY_Ocircumflexhook
  toEnum 0x1001ed5 = GDK_KEY_ocircumflexhook
  toEnum 0x1001ed6 = GDK_KEY_Ocircumflextilde
  toEnum 0x1001ed7 = GDK_KEY_ocircumflextilde
  toEnum 0x1001ed8 = GDK_KEY_Ocircumflexbelowdot
  toEnum 0x1001ed9 = GDK_KEY_ocircumflexbelowdot
  toEnum 0x1001eda = GDK_KEY_Ohornacute
  toEnum 0x1001edb = GDK_KEY_ohornacute
  toEnum 0x1001edc = GDK_KEY_Ohorngrave
  toEnum 0x1001edd = GDK_KEY_ohorngrave
  toEnum 0x1001ede = GDK_KEY_Ohornhook
  toEnum 0x1001edf = GDK_KEY_ohornhook
  toEnum 0x1001ee0 = GDK_KEY_Ohorntilde
  toEnum 0x1001ee1 = GDK_KEY_ohorntilde
  toEnum 0x1001ee2 = GDK_KEY_Ohornbelowdot
  toEnum 0x1001ee3 = GDK_KEY_ohornbelowdot
  toEnum 0x1001ee4 = GDK_KEY_Ubelowdot
  toEnum 0x1001ee5 = GDK_KEY_ubelowdot
  toEnum 0x1001ee6 = GDK_KEY_Uhook
  toEnum 0x1001ee7 = GDK_KEY_uhook
  toEnum 0x1001ee8 = GDK_KEY_Uhornacute
  toEnum 0x1001ee9 = GDK_KEY_uhornacute
  toEnum 0x1001eea = GDK_KEY_Uhorngrave
  toEnum 0x1001eeb = GDK_KEY_uhorngrave
  toEnum 0x1001eec = GDK_KEY_Uhornhook
  toEnum 0x1001eed = GDK_KEY_uhornhook
  toEnum 0x1001eee = GDK_KEY_Uhorntilde
  toEnum 0x1001eef = GDK_KEY_uhorntilde
  toEnum 0x1001ef0 = GDK_KEY_Uhornbelowdot
  toEnum 0x1001ef1 = GDK_KEY_uhornbelowdot
  toEnum 0x1001ef2 = GDK_KEY_Ygrave
  toEnum 0x1001ef3 = GDK_KEY_ygrave
  toEnum 0x1001ef4 = GDK_KEY_Ybelowdot
  toEnum 0x1001ef5 = GDK_KEY_ybelowdot
  toEnum 0x1001ef6 = GDK_KEY_Yhook
  toEnum 0x1001ef7 = GDK_KEY_yhook
  toEnum 0x1001ef8 = GDK_KEY_Ytilde
  toEnum 0x1001ef9 = GDK_KEY_ytilde
  toEnum 0x1002070 = GDK_KEY_zerosuperior
  toEnum 0x1002074 = GDK_KEY_foursuperior
  toEnum 0x1002075 = GDK_KEY_fivesuperior
  toEnum 0x1002076 = GDK_KEY_sixsuperior
  toEnum 0x1002077 = GDK_KEY_sevensuperior
  toEnum 0x1002078 = GDK_KEY_eightsuperior
  toEnum 0x1002079 = GDK_KEY_ninesuperior
  toEnum 0x1002080 = GDK_KEY_zerosubscript
  toEnum 0x1002081 = GDK_KEY_onesubscript
  toEnum 0x1002082 = GDK_KEY_twosubscript
  toEnum 0x1002083 = GDK_KEY_threesubscript
  toEnum 0x1002084 = GDK_KEY_foursubscript
  toEnum 0x1002085 = GDK_KEY_fivesubscript
  toEnum 0x1002086 = GDK_KEY_sixsubscript
  toEnum 0x1002087 = GDK_KEY_sevensubscript
  toEnum 0x1002088 = GDK_KEY_eightsubscript
  toEnum 0x1002089 = GDK_KEY_ninesubscript
  toEnum 0x10020a0 = GDK_KEY_EcuSign
  toEnum 0x10020a1 = GDK_KEY_ColonSign
  toEnum 0x10020a2 = GDK_KEY_CruzeiroSign
  toEnum 0x10020a3 = GDK_KEY_FFrancSign
  toEnum 0x10020a4 = GDK_KEY_LiraSign
  toEnum 0x10020a5 = GDK_KEY_MillSign
  toEnum 0x10020a6 = GDK_KEY_NairaSign
  toEnum 0x10020a7 = GDK_KEY_PesetaSign
  toEnum 0x10020a8 = GDK_KEY_RupeeSign
  toEnum 0x10020a9 = GDK_KEY_WonSign
  toEnum 0x10020aa = GDK_KEY_NewSheqelSign
  toEnum 0x10020ab = GDK_KEY_DongSign
  toEnum 0x1002202 = GDK_KEY_partdifferential
  toEnum 0x1002205 = GDK_KEY_emptyset
  toEnum 0x1002208 = GDK_KEY_elementof
  toEnum 0x1002209 = GDK_KEY_notelementof
  toEnum 0x100220b = GDK_KEY_containsas
  toEnum 0x100221a = GDK_KEY_squareroot
  toEnum 0x100221b = GDK_KEY_cuberoot
  toEnum 0x100221c = GDK_KEY_fourthroot
  toEnum 0x100222c = GDK_KEY_dintegral
  toEnum 0x100222d = GDK_KEY_tintegral
  toEnum 0x1002235 = GDK_KEY_because
  toEnum 0x1002247 = GDK_KEY_notapproxeq
  toEnum 0x1002248 = GDK_KEY_approxeq
  toEnum 0x1002262 = GDK_KEY_notidentical
  toEnum 0x1002263 = GDK_KEY_stricteq
  toEnum 0x1002800 = GDK_KEY_braille_blank
  toEnum 0x1002801 = GDK_KEY_braille_dots_1
  toEnum 0x1002802 = GDK_KEY_braille_dots_2
  toEnum 0x1002803 = GDK_KEY_braille_dots_12
  toEnum 0x1002804 = GDK_KEY_braille_dots_3
  toEnum 0x1002805 = GDK_KEY_braille_dots_13
  toEnum 0x1002806 = GDK_KEY_braille_dots_23
  toEnum 0x1002807 = GDK_KEY_braille_dots_123
  toEnum 0x1002808 = GDK_KEY_braille_dots_4
  toEnum 0x1002809 = GDK_KEY_braille_dots_14
  toEnum 0x100280a = GDK_KEY_braille_dots_24
  toEnum 0x100280b = GDK_KEY_braille_dots_124
  toEnum 0x100280c = GDK_KEY_braille_dots_34
  toEnum 0x100280d = GDK_KEY_braille_dots_134
  toEnum 0x100280e = GDK_KEY_braille_dots_234
  toEnum 0x100280f = GDK_KEY_braille_dots_1234
  toEnum 0x1002810 = GDK_KEY_braille_dots_5
  toEnum 0x1002811 = GDK_KEY_braille_dots_15
  toEnum 0x1002812 = GDK_KEY_braille_dots_25
  toEnum 0x1002813 = GDK_KEY_braille_dots_125
  toEnum 0x1002814 = GDK_KEY_braille_dots_35
  toEnum 0x1002815 = GDK_KEY_braille_dots_135
  toEnum 0x1002816 = GDK_KEY_braille_dots_235
  toEnum 0x1002817 = GDK_KEY_braille_dots_1235
  toEnum 0x1002818 = GDK_KEY_braille_dots_45
  toEnum 0x1002819 = GDK_KEY_braille_dots_145
  toEnum 0x100281a = GDK_KEY_braille_dots_245
  toEnum 0x100281b = GDK_KEY_braille_dots_1245
  toEnum 0x100281c = GDK_KEY_braille_dots_345
  toEnum 0x100281d = GDK_KEY_braille_dots_1345
  toEnum 0x100281e = GDK_KEY_braille_dots_2345
  toEnum 0x100281f = GDK_KEY_braille_dots_12345
  toEnum 0x1002820 = GDK_KEY_braille_dots_6
  toEnum 0x1002821 = GDK_KEY_braille_dots_16
  toEnum 0x1002822 = GDK_KEY_braille_dots_26
  toEnum 0x1002823 = GDK_KEY_braille_dots_126
  toEnum 0x1002824 = GDK_KEY_braille_dots_36
  toEnum 0x1002825 = GDK_KEY_braille_dots_136
  toEnum 0x1002826 = GDK_KEY_braille_dots_236
  toEnum 0x1002827 = GDK_KEY_braille_dots_1236
  toEnum 0x1002828 = GDK_KEY_braille_dots_46
  toEnum 0x1002829 = GDK_KEY_braille_dots_146
  toEnum 0x100282a = GDK_KEY_braille_dots_246
  toEnum 0x100282b = GDK_KEY_braille_dots_1246
  toEnum 0x100282c = GDK_KEY_braille_dots_346
  toEnum 0x100282d = GDK_KEY_braille_dots_1346
  toEnum 0x100282e = GDK_KEY_braille_dots_2346
  toEnum 0x100282f = GDK_KEY_braille_dots_12346
  toEnum 0x1002830 = GDK_KEY_braille_dots_56
  toEnum 0x1002831 = GDK_KEY_braille_dots_156
  toEnum 0x1002832 = GDK_KEY_braille_dots_256
  toEnum 0x1002833 = GDK_KEY_braille_dots_1256
  toEnum 0x1002834 = GDK_KEY_braille_dots_356
  toEnum 0x1002835 = GDK_KEY_braille_dots_1356
  toEnum 0x1002836 = GDK_KEY_braille_dots_2356
  toEnum 0x1002837 = GDK_KEY_braille_dots_12356
  toEnum 0x1002838 = GDK_KEY_braille_dots_456
  toEnum 0x1002839 = GDK_KEY_braille_dots_1456
  toEnum 0x100283a = GDK_KEY_braille_dots_2456
  toEnum 0x100283b = GDK_KEY_braille_dots_12456
  toEnum 0x100283c = GDK_KEY_braille_dots_3456
  toEnum 0x100283d = GDK_KEY_braille_dots_13456
  toEnum 0x100283e = GDK_KEY_braille_dots_23456
  toEnum 0x100283f = GDK_KEY_braille_dots_123456
  toEnum 0x1002840 = GDK_KEY_braille_dots_7
  toEnum 0x1002841 = GDK_KEY_braille_dots_17
  toEnum 0x1002842 = GDK_KEY_braille_dots_27
  toEnum 0x1002843 = GDK_KEY_braille_dots_127
  toEnum 0x1002844 = GDK_KEY_braille_dots_37
  toEnum 0x1002845 = GDK_KEY_braille_dots_137
  toEnum 0x1002846 = GDK_KEY_braille_dots_237
  toEnum 0x1002847 = GDK_KEY_braille_dots_1237
  toEnum 0x1002848 = GDK_KEY_braille_dots_47
  toEnum 0x1002849 = GDK_KEY_braille_dots_147
  toEnum 0x100284a = GDK_KEY_braille_dots_247
  toEnum 0x100284b = GDK_KEY_braille_dots_1247
  toEnum 0x100284c = GDK_KEY_braille_dots_347
  toEnum 0x100284d = GDK_KEY_braille_dots_1347
  toEnum 0x100284e = GDK_KEY_braille_dots_2347
  toEnum 0x100284f = GDK_KEY_braille_dots_12347
  toEnum 0x1002850 = GDK_KEY_braille_dots_57
  toEnum 0x1002851 = GDK_KEY_braille_dots_157
  toEnum 0x1002852 = GDK_KEY_braille_dots_257
  toEnum 0x1002853 = GDK_KEY_braille_dots_1257
  toEnum 0x1002854 = GDK_KEY_braille_dots_357
  toEnum 0x1002855 = GDK_KEY_braille_dots_1357
  toEnum 0x1002856 = GDK_KEY_braille_dots_2357
  toEnum 0x1002857 = GDK_KEY_braille_dots_12357
  toEnum 0x1002858 = GDK_KEY_braille_dots_457
  toEnum 0x1002859 = GDK_KEY_braille_dots_1457
  toEnum 0x100285a = GDK_KEY_braille_dots_2457
  toEnum 0x100285b = GDK_KEY_braille_dots_12457
  toEnum 0x100285c = GDK_KEY_braille_dots_3457
  toEnum 0x100285d = GDK_KEY_braille_dots_13457
  toEnum 0x100285e = GDK_KEY_braille_dots_23457
  toEnum 0x100285f = GDK_KEY_braille_dots_123457
  toEnum 0x1002860 = GDK_KEY_braille_dots_67
  toEnum 0x1002861 = GDK_KEY_braille_dots_167
  toEnum 0x1002862 = GDK_KEY_braille_dots_267
  toEnum 0x1002863 = GDK_KEY_braille_dots_1267
  toEnum 0x1002864 = GDK_KEY_braille_dots_367
  toEnum 0x1002865 = GDK_KEY_braille_dots_1367
  toEnum 0x1002866 = GDK_KEY_braille_dots_2367
  toEnum 0x1002867 = GDK_KEY_braille_dots_12367
  toEnum 0x1002868 = GDK_KEY_braille_dots_467
  toEnum 0x1002869 = GDK_KEY_braille_dots_1467
  toEnum 0x100286a = GDK_KEY_braille_dots_2467
  toEnum 0x100286b = GDK_KEY_braille_dots_12467
  toEnum 0x100286c = GDK_KEY_braille_dots_3467
  toEnum 0x100286d = GDK_KEY_braille_dots_13467
  toEnum 0x100286e = GDK_KEY_braille_dots_23467
  toEnum 0x100286f = GDK_KEY_braille_dots_123467
  toEnum 0x1002870 = GDK_KEY_braille_dots_567
  toEnum 0x1002871 = GDK_KEY_braille_dots_1567
  toEnum 0x1002872 = GDK_KEY_braille_dots_2567
  toEnum 0x1002873 = GDK_KEY_braille_dots_12567
  toEnum 0x1002874 = GDK_KEY_braille_dots_3567
  toEnum 0x1002875 = GDK_KEY_braille_dots_13567
  toEnum 0x1002876 = GDK_KEY_braille_dots_23567
  toEnum 0x1002877 = GDK_KEY_braille_dots_123567
  toEnum 0x1002878 = GDK_KEY_braille_dots_4567
  toEnum 0x1002879 = GDK_KEY_braille_dots_14567
  toEnum 0x100287a = GDK_KEY_braille_dots_24567
  toEnum 0x100287b = GDK_KEY_braille_dots_124567
  toEnum 0x100287c = GDK_KEY_braille_dots_34567
  toEnum 0x100287d = GDK_KEY_braille_dots_134567
  toEnum 0x100287e = GDK_KEY_braille_dots_234567
  toEnum 0x100287f = GDK_KEY_braille_dots_1234567
  toEnum 0x1002880 = GDK_KEY_braille_dots_8
  toEnum 0x1002881 = GDK_KEY_braille_dots_18
  toEnum 0x1002882 = GDK_KEY_braille_dots_28
  toEnum 0x1002883 = GDK_KEY_braille_dots_128
  toEnum 0x1002884 = GDK_KEY_braille_dots_38
  toEnum 0x1002885 = GDK_KEY_braille_dots_138
  toEnum 0x1002886 = GDK_KEY_braille_dots_238
  toEnum 0x1002887 = GDK_KEY_braille_dots_1238
  toEnum 0x1002888 = GDK_KEY_braille_dots_48
  toEnum 0x1002889 = GDK_KEY_braille_dots_148
  toEnum 0x100288a = GDK_KEY_braille_dots_248
  toEnum 0x100288b = GDK_KEY_braille_dots_1248
  toEnum 0x100288c = GDK_KEY_braille_dots_348
  toEnum 0x100288d = GDK_KEY_braille_dots_1348
  toEnum 0x100288e = GDK_KEY_braille_dots_2348
  toEnum 0x100288f = GDK_KEY_braille_dots_12348
  toEnum 0x1002890 = GDK_KEY_braille_dots_58
  toEnum 0x1002891 = GDK_KEY_braille_dots_158
  toEnum 0x1002892 = GDK_KEY_braille_dots_258
  toEnum 0x1002893 = GDK_KEY_braille_dots_1258
  toEnum 0x1002894 = GDK_KEY_braille_dots_358
  toEnum 0x1002895 = GDK_KEY_braille_dots_1358
  toEnum 0x1002896 = GDK_KEY_braille_dots_2358
  toEnum 0x1002897 = GDK_KEY_braille_dots_12358
  toEnum 0x1002898 = GDK_KEY_braille_dots_458
  toEnum 0x1002899 = GDK_KEY_braille_dots_1458
  toEnum 0x100289a = GDK_KEY_braille_dots_2458
  toEnum 0x100289b = GDK_KEY_braille_dots_12458
  toEnum 0x100289c = GDK_KEY_braille_dots_3458
  toEnum 0x100289d = GDK_KEY_braille_dots_13458
  toEnum 0x100289e = GDK_KEY_braille_dots_23458
  toEnum 0x100289f = GDK_KEY_braille_dots_123458
  toEnum 0x10028a0 = GDK_KEY_braille_dots_68
  toEnum 0x10028a1 = GDK_KEY_braille_dots_168
  toEnum 0x10028a2 = GDK_KEY_braille_dots_268
  toEnum 0x10028a3 = GDK_KEY_braille_dots_1268
  toEnum 0x10028a4 = GDK_KEY_braille_dots_368
  toEnum 0x10028a5 = GDK_KEY_braille_dots_1368
  toEnum 0x10028a6 = GDK_KEY_braille_dots_2368
  toEnum 0x10028a7 = GDK_KEY_braille_dots_12368
  toEnum 0x10028a8 = GDK_KEY_braille_dots_468
  toEnum 0x10028a9 = GDK_KEY_braille_dots_1468
  toEnum 0x10028aa = GDK_KEY_braille_dots_2468
  toEnum 0x10028ab = GDK_KEY_braille_dots_12468
  toEnum 0x10028ac = GDK_KEY_braille_dots_3468
  toEnum 0x10028ad = GDK_KEY_braille_dots_13468
  toEnum 0x10028ae = GDK_KEY_braille_dots_23468
  toEnum 0x10028af = GDK_KEY_braille_dots_123468
  toEnum 0x10028b0 = GDK_KEY_braille_dots_568
  toEnum 0x10028b1 = GDK_KEY_braille_dots_1568
  toEnum 0x10028b2 = GDK_KEY_braille_dots_2568
  toEnum 0x10028b3 = GDK_KEY_braille_dots_12568
  toEnum 0x10028b4 = GDK_KEY_braille_dots_3568
  toEnum 0x10028b5 = GDK_KEY_braille_dots_13568
  toEnum 0x10028b6 = GDK_KEY_braille_dots_23568
  toEnum 0x10028b7 = GDK_KEY_braille_dots_123568
  toEnum 0x10028b8 = GDK_KEY_braille_dots_4568
  toEnum 0x10028b9 = GDK_KEY_braille_dots_14568
  toEnum 0x10028ba = GDK_KEY_braille_dots_24568
  toEnum 0x10028bb = GDK_KEY_braille_dots_124568
  toEnum 0x10028bc = GDK_KEY_braille_dots_34568
  toEnum 0x10028bd = GDK_KEY_braille_dots_134568
  toEnum 0x10028be = GDK_KEY_braille_dots_234568
  toEnum 0x10028bf = GDK_KEY_braille_dots_1234568
  toEnum 0x10028c0 = GDK_KEY_braille_dots_78
  toEnum 0x10028c1 = GDK_KEY_braille_dots_178
  toEnum 0x10028c2 = GDK_KEY_braille_dots_278
  toEnum 0x10028c3 = GDK_KEY_braille_dots_1278
  toEnum 0x10028c4 = GDK_KEY_braille_dots_378
  toEnum 0x10028c5 = GDK_KEY_braille_dots_1378
  toEnum 0x10028c6 = GDK_KEY_braille_dots_2378
  toEnum 0x10028c7 = GDK_KEY_braille_dots_12378
  toEnum 0x10028c8 = GDK_KEY_braille_dots_478
  toEnum 0x10028c9 = GDK_KEY_braille_dots_1478
  toEnum 0x10028ca = GDK_KEY_braille_dots_2478
  toEnum 0x10028cb = GDK_KEY_braille_dots_12478
  toEnum 0x10028cc = GDK_KEY_braille_dots_3478
  toEnum 0x10028cd = GDK_KEY_braille_dots_13478
  toEnum 0x10028ce = GDK_KEY_braille_dots_23478
  toEnum 0x10028cf = GDK_KEY_braille_dots_123478
  toEnum 0x10028d0 = GDK_KEY_braille_dots_578
  toEnum 0x10028d1 = GDK_KEY_braille_dots_1578
  toEnum 0x10028d2 = GDK_KEY_braille_dots_2578
  toEnum 0x10028d3 = GDK_KEY_braille_dots_12578
  toEnum 0x10028d4 = GDK_KEY_braille_dots_3578
  toEnum 0x10028d5 = GDK_KEY_braille_dots_13578
  toEnum 0x10028d6 = GDK_KEY_braille_dots_23578
  toEnum 0x10028d7 = GDK_KEY_braille_dots_123578
  toEnum 0x10028d8 = GDK_KEY_braille_dots_4578
  toEnum 0x10028d9 = GDK_KEY_braille_dots_14578
  toEnum 0x10028da = GDK_KEY_braille_dots_24578
  toEnum 0x10028db = GDK_KEY_braille_dots_124578
  toEnum 0x10028dc = GDK_KEY_braille_dots_34578
  toEnum 0x10028dd = GDK_KEY_braille_dots_134578
  toEnum 0x10028de = GDK_KEY_braille_dots_234578
  toEnum 0x10028df = GDK_KEY_braille_dots_1234578
  toEnum 0x10028e0 = GDK_KEY_braille_dots_678
  toEnum 0x10028e1 = GDK_KEY_braille_dots_1678
  toEnum 0x10028e2 = GDK_KEY_braille_dots_2678
  toEnum 0x10028e3 = GDK_KEY_braille_dots_12678
  toEnum 0x10028e4 = GDK_KEY_braille_dots_3678
  toEnum 0x10028e5 = GDK_KEY_braille_dots_13678
  toEnum 0x10028e6 = GDK_KEY_braille_dots_23678
  toEnum 0x10028e7 = GDK_KEY_braille_dots_123678
  toEnum 0x10028e8 = GDK_KEY_braille_dots_4678
  toEnum 0x10028e9 = GDK_KEY_braille_dots_14678
  toEnum 0x10028ea = GDK_KEY_braille_dots_24678
  toEnum 0x10028eb = GDK_KEY_braille_dots_124678
  toEnum 0x10028ec = GDK_KEY_braille_dots_34678
  toEnum 0x10028ed = GDK_KEY_braille_dots_134678
  toEnum 0x10028ee = GDK_KEY_braille_dots_234678
  toEnum 0x10028ef = GDK_KEY_braille_dots_1234678
  toEnum 0x10028f0 = GDK_KEY_braille_dots_5678
  toEnum 0x10028f1 = GDK_KEY_braille_dots_15678
  toEnum 0x10028f2 = GDK_KEY_braille_dots_25678
  toEnum 0x10028f3 = GDK_KEY_braille_dots_125678
  toEnum 0x10028f4 = GDK_KEY_braille_dots_35678
  toEnum 0x10028f5 = GDK_KEY_braille_dots_135678
  toEnum 0x10028f6 = GDK_KEY_braille_dots_235678
  toEnum 0x10028f7 = GDK_KEY_braille_dots_1235678
  toEnum 0x10028f8 = GDK_KEY_braille_dots_45678
  toEnum 0x10028f9 = GDK_KEY_braille_dots_145678
  toEnum 0x10028fa = GDK_KEY_braille_dots_245678
  toEnum 0x10028fb = GDK_KEY_braille_dots_1245678
  toEnum 0x10028fc = GDK_KEY_braille_dots_345678
  toEnum 0x10028fd = GDK_KEY_braille_dots_1345678
  toEnum 0x10028fe = GDK_KEY_braille_dots_2345678
  toEnum 0x10028ff = GDK_KEY_braille_dots_12345678
  toEnum 0x1008fe01 = GDK_KEY_Switch_VT_1
  toEnum 0x1008fe02 = GDK_KEY_Switch_VT_2
  toEnum 0x1008fe03 = GDK_KEY_Switch_VT_3
  toEnum 0x1008fe04 = GDK_KEY_Switch_VT_4
  toEnum 0x1008fe05 = GDK_KEY_Switch_VT_5
  toEnum 0x1008fe06 = GDK_KEY_Switch_VT_6
  toEnum 0x1008fe07 = GDK_KEY_Switch_VT_7
  toEnum 0x1008fe08 = GDK_KEY_Switch_VT_8
  toEnum 0x1008fe09 = GDK_KEY_Switch_VT_9
  toEnum 0x1008fe0a = GDK_KEY_Switch_VT_10
  toEnum 0x1008fe0b = GDK_KEY_Switch_VT_11
  toEnum 0x1008fe0c = GDK_KEY_Switch_VT_12
  toEnum 0x1008fe20 = GDK_KEY_Ungrab
  toEnum 0x1008fe21 = GDK_KEY_ClearGrab
  toEnum 0x1008fe22 = GDK_KEY_Next_VMode
  toEnum 0x1008fe23 = GDK_KEY_Prev_VMode
  toEnum 0x1008fe24 = GDK_KEY_LogWindowTree
  toEnum 0x1008fe25 = GDK_KEY_LogGrabInfo
  toEnum 0x1008ff01 = GDK_KEY_ModeLock
  toEnum 0x1008ff02 = GDK_KEY_MonBrightnessUp
  toEnum 0x1008ff03 = GDK_KEY_MonBrightnessDown
  toEnum 0x1008ff04 = GDK_KEY_KbdLightOnOff
  toEnum 0x1008ff05 = GDK_KEY_KbdBrightnessUp
  toEnum 0x1008ff06 = GDK_KEY_KbdBrightnessDown
  toEnum 0x1008ff10 = GDK_KEY_Standby
  toEnum 0x1008ff11 = GDK_KEY_AudioLowerVolume
  toEnum 0x1008ff12 = GDK_KEY_AudioMute
  toEnum 0x1008ff13 = GDK_KEY_AudioRaiseVolume
  toEnum 0x1008ff14 = GDK_KEY_AudioPlay
  toEnum 0x1008ff15 = GDK_KEY_AudioStop
  toEnum 0x1008ff16 = GDK_KEY_AudioPrev
  toEnum 0x1008ff17 = GDK_KEY_AudioNext
  toEnum 0x1008ff18 = GDK_KEY_HomePage
  toEnum 0x1008ff19 = GDK_KEY_Mail
  toEnum 0x1008ff1a = GDK_KEY_Start
  toEnum 0x1008ff1b = GDK_KEY_Search
  toEnum 0x1008ff1c = GDK_KEY_AudioRecord
  toEnum 0x1008ff1d = GDK_KEY_Calculator
  toEnum 0x1008ff1e = GDK_KEY_Memo
  toEnum 0x1008ff1f = GDK_KEY_ToDoList
  toEnum 0x1008ff20 = GDK_KEY_Calendar
  toEnum 0x1008ff21 = GDK_KEY_PowerDown
  toEnum 0x1008ff22 = GDK_KEY_ContrastAdjust
  toEnum 0x1008ff23 = GDK_KEY_RockerUp
  toEnum 0x1008ff24 = GDK_KEY_RockerDown
  toEnum 0x1008ff25 = GDK_KEY_RockerEnter
  toEnum 0x1008ff26 = GDK_KEY_Back
  toEnum 0x1008ff27 = GDK_KEY_Forward
  toEnum 0x1008ff28 = GDK_KEY_Stop
  toEnum 0x1008ff29 = GDK_KEY_Refresh
  toEnum 0x1008ff2a = GDK_KEY_PowerOff
  toEnum 0x1008ff2b = GDK_KEY_WakeUp
  toEnum 0x1008ff2c = GDK_KEY_Eject
  toEnum 0x1008ff2d = GDK_KEY_ScreenSaver
  toEnum 0x1008ff2e = GDK_KEY_WWW
  toEnum 0x1008ff2f = GDK_KEY_Sleep
  toEnum 0x1008ff30 = GDK_KEY_Favorites
  toEnum 0x1008ff31 = GDK_KEY_AudioPause
  toEnum 0x1008ff32 = GDK_KEY_AudioMedia
  toEnum 0x1008ff33 = GDK_KEY_MyComputer
  toEnum 0x1008ff34 = GDK_KEY_VendorHome
  toEnum 0x1008ff35 = GDK_KEY_LightBulb
  toEnum 0x1008ff36 = GDK_KEY_Shop
  toEnum 0x1008ff37 = GDK_KEY_History
  toEnum 0x1008ff38 = GDK_KEY_OpenURL
  toEnum 0x1008ff39 = GDK_KEY_AddFavorite
  toEnum 0x1008ff3a = GDK_KEY_HotLinks
  toEnum 0x1008ff3b = GDK_KEY_BrightnessAdjust
  toEnum 0x1008ff3c = GDK_KEY_Finance
  toEnum 0x1008ff3d = GDK_KEY_Community
  toEnum 0x1008ff3e = GDK_KEY_AudioRewind
  toEnum 0x1008ff3f = GDK_KEY_BackForward
  toEnum 0x1008ff40 = GDK_KEY_Launch0
  toEnum 0x1008ff41 = GDK_KEY_Launch1
  toEnum 0x1008ff42 = GDK_KEY_Launch2
  toEnum 0x1008ff43 = GDK_KEY_Launch3
  toEnum 0x1008ff44 = GDK_KEY_Launch4
  toEnum 0x1008ff45 = GDK_KEY_Launch5
  toEnum 0x1008ff46 = GDK_KEY_Launch6
  toEnum 0x1008ff47 = GDK_KEY_Launch7
  toEnum 0x1008ff48 = GDK_KEY_Launch8
  toEnum 0x1008ff49 = GDK_KEY_Launch9
  toEnum 0x1008ff4a = GDK_KEY_LaunchA
  toEnum 0x1008ff4b = GDK_KEY_LaunchB
  toEnum 0x1008ff4c = GDK_KEY_LaunchC
  toEnum 0x1008ff4d = GDK_KEY_LaunchD
  toEnum 0x1008ff4e = GDK_KEY_LaunchE
  toEnum 0x1008ff4f = GDK_KEY_LaunchF
  toEnum 0x1008ff50 = GDK_KEY_ApplicationLeft
  toEnum 0x1008ff51 = GDK_KEY_ApplicationRight
  toEnum 0x1008ff52 = GDK_KEY_Book
  toEnum 0x1008ff53 = GDK_KEY_CD
  toEnum 0x1008ff55 = GDK_KEY_WindowClear
  toEnum 0x1008ff56 = GDK_KEY_Close
  toEnum 0x1008ff57 = GDK_KEY_Copy
  toEnum 0x1008ff58 = GDK_KEY_Cut
  toEnum 0x1008ff59 = GDK_KEY_Display
  toEnum 0x1008ff5a = GDK_KEY_DOS
  toEnum 0x1008ff5b = GDK_KEY_Documents
  toEnum 0x1008ff5c = GDK_KEY_Excel
  toEnum 0x1008ff5d = GDK_KEY_Explorer
  toEnum 0x1008ff5e = GDK_KEY_Game
  toEnum 0x1008ff5f = GDK_KEY_Go
  toEnum 0x1008ff60 = GDK_KEY_iTouch
  toEnum 0x1008ff61 = GDK_KEY_LogOff
  toEnum 0x1008ff62 = GDK_KEY_Market
  toEnum 0x1008ff63 = GDK_KEY_Meeting
  toEnum 0x1008ff65 = GDK_KEY_MenuKB
  toEnum 0x1008ff66 = GDK_KEY_MenuPB
  toEnum 0x1008ff67 = GDK_KEY_MySites
  toEnum 0x1008ff68 = GDK_KEY_New
  toEnum 0x1008ff69 = GDK_KEY_News
  toEnum 0x1008ff6a = GDK_KEY_OfficeHome
  toEnum 0x1008ff6b = GDK_KEY_Open
  toEnum 0x1008ff6c = GDK_KEY_Option
  toEnum 0x1008ff6d = GDK_KEY_Paste
  toEnum 0x1008ff6e = GDK_KEY_Phone
  toEnum 0x1008ff72 = GDK_KEY_Reply
  toEnum 0x1008ff73 = GDK_KEY_Reload
  toEnum 0x1008ff74 = GDK_KEY_RotateWindows
  toEnum 0x1008ff75 = GDK_KEY_RotationPB
  toEnum 0x1008ff76 = GDK_KEY_RotationKB
  toEnum 0x1008ff77 = GDK_KEY_Save
  toEnum 0x1008ff78 = GDK_KEY_ScrollUp
  toEnum 0x1008ff79 = GDK_KEY_ScrollDown
  toEnum 0x1008ff7a = GDK_KEY_ScrollClick
  toEnum 0x1008ff7b = GDK_KEY_Send
  toEnum 0x1008ff7c = GDK_KEY_Spell
  toEnum 0x1008ff7d = GDK_KEY_SplitScreen
  toEnum 0x1008ff7e = GDK_KEY_Support
  toEnum 0x1008ff7f = GDK_KEY_TaskPane
  toEnum 0x1008ff80 = GDK_KEY_Terminal
  toEnum 0x1008ff81 = GDK_KEY_Tools
  toEnum 0x1008ff82 = GDK_KEY_Travel
  toEnum 0x1008ff84 = GDK_KEY_UserPB
  toEnum 0x1008ff85 = GDK_KEY_User1KB
  toEnum 0x1008ff86 = GDK_KEY_User2KB
  toEnum 0x1008ff87 = GDK_KEY_Video
  toEnum 0x1008ff88 = GDK_KEY_WheelButton
  toEnum 0x1008ff89 = GDK_KEY_Word
  toEnum 0x1008ff8a = GDK_KEY_Xfer
  toEnum 0x1008ff8b = GDK_KEY_ZoomIn
  toEnum 0x1008ff8c = GDK_KEY_ZoomOut
  toEnum 0x1008ff8d = GDK_KEY_Away
  toEnum 0x1008ff8e = GDK_KEY_Messenger
  toEnum 0x1008ff8f = GDK_KEY_WebCam
  toEnum 0x1008ff90 = GDK_KEY_MailForward
  toEnum 0x1008ff91 = GDK_KEY_Pictures
  toEnum 0x1008ff92 = GDK_KEY_Music
  toEnum 0x1008ff93 = GDK_KEY_Battery
  toEnum 0x1008ff94 = GDK_KEY_Bluetooth
  toEnum 0x1008ff95 = GDK_KEY_WLAN
  toEnum 0x1008ff96 = GDK_KEY_UWB
  toEnum 0x1008ff97 = GDK_KEY_AudioForward
  toEnum 0x1008ff98 = GDK_KEY_AudioRepeat
  toEnum 0x1008ff99 = GDK_KEY_AudioRandomPlay
  toEnum 0x1008ff9a = GDK_KEY_Subtitle
  toEnum 0x1008ff9b = GDK_KEY_AudioCycleTrack
  toEnum 0x1008ff9c = GDK_KEY_CycleAngle
  toEnum 0x1008ff9d = GDK_KEY_FrameBack
  toEnum 0x1008ff9e = GDK_KEY_FrameForward
  toEnum 0x1008ff9f = GDK_KEY_Time
  toEnum 0x1008ffa0 = GDK_KEY_SelectButton
  toEnum 0x1008ffa1 = GDK_KEY_View
  toEnum 0x1008ffa2 = GDK_KEY_TopMenu
  toEnum 0x1008ffa3 = GDK_KEY_Red
  toEnum 0x1008ffa4 = GDK_KEY_Green
  toEnum 0x1008ffa5 = GDK_KEY_Yellow
  toEnum 0x1008ffa6 = GDK_KEY_Blue
  toEnum 0x1008ffa7 = GDK_KEY_Suspend
  toEnum 0x1008ffa8 = GDK_KEY_Hibernate
  toEnum 0x1008ffa9 = GDK_KEY_TouchpadToggle
  toEnum 0x1008ffb0 = GDK_KEY_TouchpadOn
  toEnum 0x1008ffb1 = GDK_KEY_TouchpadOff
  toEnum 0x1008ffb2 = GDK_KEY_AudioMicMute
  toEnum 0x13bc = GDK_KEY_OE
  toEnum 0x13bd = GDK_KEY_oe
  toEnum 0x13be = GDK_KEY_Ydiaeresis
  toEnum 0x1a1 = GDK_KEY_Aogonek
  toEnum 0x1a2 = GDK_KEY_breve
  toEnum 0x1a3 = GDK_KEY_Lstroke
  toEnum 0x1a5 = GDK_KEY_Lcaron
  toEnum 0x1a6 = GDK_KEY_Sacute
  toEnum 0x1a9 = GDK_KEY_Scaron
  toEnum 0x1aa = GDK_KEY_Scedilla
  toEnum 0x1ab = GDK_KEY_Tcaron
  toEnum 0x1ac = GDK_KEY_Zacute
  toEnum 0x1ae = GDK_KEY_Zcaron
  toEnum 0x1af = GDK_KEY_Zabovedot
  toEnum 0x1b1 = GDK_KEY_aogonek
  toEnum 0x1b2 = GDK_KEY_ogonek
  toEnum 0x1b3 = GDK_KEY_lstroke
  toEnum 0x1b5 = GDK_KEY_lcaron
  toEnum 0x1b6 = GDK_KEY_sacute
  toEnum 0x1b7 = GDK_KEY_caron
  toEnum 0x1b9 = GDK_KEY_scaron
  toEnum 0x1ba = GDK_KEY_scedilla
  toEnum 0x1bb = GDK_KEY_tcaron
  toEnum 0x1bc = GDK_KEY_zacute
  toEnum 0x1bd = GDK_KEY_doubleacute
  toEnum 0x1be = GDK_KEY_zcaron
  toEnum 0x1bf = GDK_KEY_zabovedot
  toEnum 0x1c0 = GDK_KEY_Racute
  toEnum 0x1c3 = GDK_KEY_Abreve
  toEnum 0x1c5 = GDK_KEY_Lacute
  toEnum 0x1c6 = GDK_KEY_Cacute
  toEnum 0x1c8 = GDK_KEY_Ccaron
  toEnum 0x1ca = GDK_KEY_Eogonek
  toEnum 0x1cc = GDK_KEY_Ecaron
  toEnum 0x1cf = GDK_KEY_Dcaron
  toEnum 0x1d0 = GDK_KEY_Dstroke
  toEnum 0x1d1 = GDK_KEY_Nacute
  toEnum 0x1d2 = GDK_KEY_Ncaron
  toEnum 0x1d5 = GDK_KEY_Odoubleacute
  toEnum 0x1d8 = GDK_KEY_Rcaron
  toEnum 0x1d9 = GDK_KEY_Uring
  toEnum 0x1db = GDK_KEY_Udoubleacute
  toEnum 0x1de = GDK_KEY_Tcedilla
  toEnum 0x1e0 = GDK_KEY_racute
  toEnum 0x1e3 = GDK_KEY_abreve
  toEnum 0x1e5 = GDK_KEY_lacute
  toEnum 0x1e6 = GDK_KEY_cacute
  toEnum 0x1e8 = GDK_KEY_ccaron
  toEnum 0x1ea = GDK_KEY_eogonek
  toEnum 0x1ec = GDK_KEY_ecaron
  toEnum 0x1ef = GDK_KEY_dcaron
  toEnum 0x1f0 = GDK_KEY_dstroke
  toEnum 0x1f1 = GDK_KEY_nacute
  toEnum 0x1f2 = GDK_KEY_ncaron
  toEnum 0x1f5 = GDK_KEY_odoubleacute
  toEnum 0x1f8 = GDK_KEY_rcaron
  toEnum 0x1f9 = GDK_KEY_uring
  toEnum 0x1fb = GDK_KEY_udoubleacute
  toEnum 0x1fe = GDK_KEY_tcedilla
  toEnum 0x1ff = GDK_KEY_abovedot
  toEnum 0x20ac = GDK_KEY_EuroSign
  toEnum 0x2a1 = GDK_KEY_Hstroke
  toEnum 0x2a6 = GDK_KEY_Hcircumflex
  toEnum 0x2a9 = GDK_KEY_Iabovedot
  toEnum 0x2ab = GDK_KEY_Gbreve
  toEnum 0x2ac = GDK_KEY_Jcircumflex
  toEnum 0x2b1 = GDK_KEY_hstroke
  toEnum 0x2b6 = GDK_KEY_hcircumflex
  toEnum 0x2b9 = GDK_KEY_idotless
  toEnum 0x2bb = GDK_KEY_gbreve
  toEnum 0x2bc = GDK_KEY_jcircumflex
  toEnum 0x2c5 = GDK_KEY_Cabovedot
  toEnum 0x2c6 = GDK_KEY_Ccircumflex
  toEnum 0x2d5 = GDK_KEY_Gabovedot
  toEnum 0x2d8 = GDK_KEY_Gcircumflex
  toEnum 0x2dd = GDK_KEY_Ubreve
  toEnum 0x2de = GDK_KEY_Scircumflex
  toEnum 0x2e5 = GDK_KEY_cabovedot
  toEnum 0x2e6 = GDK_KEY_ccircumflex
  toEnum 0x2f5 = GDK_KEY_gabovedot
  toEnum 0x2f8 = GDK_KEY_gcircumflex
  toEnum 0x2fd = GDK_KEY_ubreve
  toEnum 0x2fe = GDK_KEY_scircumflex
  toEnum 0x3a2 = GDK_KEY_kappa
  toEnum 0x3a3 = GDK_KEY_Rcedilla
  toEnum 0x3a5 = GDK_KEY_Itilde
  toEnum 0x3a6 = GDK_KEY_Lcedilla
  toEnum 0x3aa = GDK_KEY_Emacron
  toEnum 0x3ab = GDK_KEY_Gcedilla
  toEnum 0x3ac = GDK_KEY_Tslash
  toEnum 0x3b3 = GDK_KEY_rcedilla
  toEnum 0x3b5 = GDK_KEY_itilde
  toEnum 0x3b6 = GDK_KEY_lcedilla
  toEnum 0x3ba = GDK_KEY_emacron
  toEnum 0x3bb = GDK_KEY_gcedilla
  toEnum 0x3bc = GDK_KEY_tslash
  toEnum 0x3bd = GDK_KEY_ENG
  toEnum 0x3bf = GDK_KEY_eng
  toEnum 0x3c0 = GDK_KEY_Amacron
  toEnum 0x3c7 = GDK_KEY_Iogonek
  toEnum 0x3cc = GDK_KEY_Eabovedot
  toEnum 0x3cf = GDK_KEY_Imacron
  toEnum 0x3d1 = GDK_KEY_Ncedilla
  toEnum 0x3d2 = GDK_KEY_Omacron
  toEnum 0x3d3 = GDK_KEY_Kcedilla
  toEnum 0x3d9 = GDK_KEY_Uogonek
  toEnum 0x3dd = GDK_KEY_Utilde
  toEnum 0x3de = GDK_KEY_Umacron
  toEnum 0x3e0 = GDK_KEY_amacron
  toEnum 0x3e7 = GDK_KEY_iogonek
  toEnum 0x3ec = GDK_KEY_eabovedot
  toEnum 0x3ef = GDK_KEY_imacron
  toEnum 0x3f1 = GDK_KEY_ncedilla
  toEnum 0x3f2 = GDK_KEY_omacron
  toEnum 0x3f3 = GDK_KEY_kcedilla
  toEnum 0x3f9 = GDK_KEY_uogonek
  toEnum 0x3fd = GDK_KEY_utilde
  toEnum 0x3fe = GDK_KEY_umacron
  toEnum 0x47e = GDK_KEY_overline
  toEnum 0x4a1 = GDK_KEY_kana_fullstop
  toEnum 0x4a2 = GDK_KEY_kana_openingbracket
  toEnum 0x4a3 = GDK_KEY_kana_closingbracket
  toEnum 0x4a4 = GDK_KEY_kana_comma
  toEnum 0x4a5 = GDK_KEY_kana_middledot
  toEnum 0x4a6 = GDK_KEY_kana_WO
  toEnum 0x4a7 = GDK_KEY_kana_a
  toEnum 0x4a8 = GDK_KEY_kana_i
  toEnum 0x4a9 = GDK_KEY_kana_u
  toEnum 0x4aa = GDK_KEY_kana_e
  toEnum 0x4ab = GDK_KEY_kana_o
  toEnum 0x4ac = GDK_KEY_kana_ya
  toEnum 0x4ad = GDK_KEY_kana_yu
  toEnum 0x4ae = GDK_KEY_kana_yo
  toEnum 0x4af = GDK_KEY_kana_tu
  toEnum 0x4b0 = GDK_KEY_prolongedsound
  toEnum 0x4b1 = GDK_KEY_kana_A
  toEnum 0x4b2 = GDK_KEY_kana_I
  toEnum 0x4b3 = GDK_KEY_kana_U
  toEnum 0x4b4 = GDK_KEY_kana_E
  toEnum 0x4b5 = GDK_KEY_kana_O
  toEnum 0x4b6 = GDK_KEY_kana_KA
  toEnum 0x4b7 = GDK_KEY_kana_KI
  toEnum 0x4b8 = GDK_KEY_kana_KU
  toEnum 0x4b9 = GDK_KEY_kana_KE
  toEnum 0x4ba = GDK_KEY_kana_KO
  toEnum 0x4bb = GDK_KEY_kana_SA
  toEnum 0x4bc = GDK_KEY_kana_SHI
  toEnum 0x4bd = GDK_KEY_kana_SU
  toEnum 0x4be = GDK_KEY_kana_SE
  toEnum 0x4bf = GDK_KEY_kana_SO
  toEnum 0x4c0 = GDK_KEY_kana_TA
  toEnum 0x4c1 = GDK_KEY_kana_CHI
  toEnum 0x4c2 = GDK_KEY_kana_TSU
  toEnum 0x4c3 = GDK_KEY_kana_TE
  toEnum 0x4c4 = GDK_KEY_kana_TO
  toEnum 0x4c5 = GDK_KEY_kana_NA
  toEnum 0x4c6 = GDK_KEY_kana_NI
  toEnum 0x4c7 = GDK_KEY_kana_NU
  toEnum 0x4c8 = GDK_KEY_kana_NE
  toEnum 0x4c9 = GDK_KEY_kana_NO
  toEnum 0x4ca = GDK_KEY_kana_HA
  toEnum 0x4cb = GDK_KEY_kana_HI
  toEnum 0x4cc = GDK_KEY_kana_FU
  toEnum 0x4cd = GDK_KEY_kana_HE
  toEnum 0x4ce = GDK_KEY_kana_HO
  toEnum 0x4cf = GDK_KEY_kana_MA
  toEnum 0x4d0 = GDK_KEY_kana_MI
  toEnum 0x4d1 = GDK_KEY_kana_MU
  toEnum 0x4d2 = GDK_KEY_kana_ME
  toEnum 0x4d3 = GDK_KEY_kana_MO
  toEnum 0x4d4 = GDK_KEY_kana_YA
  toEnum 0x4d5 = GDK_KEY_kana_YU
  toEnum 0x4d6 = GDK_KEY_kana_YO
  toEnum 0x4d7 = GDK_KEY_kana_RA
  toEnum 0x4d8 = GDK_KEY_kana_RI
  toEnum 0x4d9 = GDK_KEY_kana_RU
  toEnum 0x4da = GDK_KEY_kana_RE
  toEnum 0x4db = GDK_KEY_kana_RO
  toEnum 0x4dc = GDK_KEY_kana_WA
  toEnum 0x4dd = GDK_KEY_kana_N
  toEnum 0x4de = GDK_KEY_voicedsound
  toEnum 0x4df = GDK_KEY_semivoicedsound
  toEnum 0x5ac = GDK_KEY_Arabic_comma
  toEnum 0x5bb = GDK_KEY_Arabic_semicolon
  toEnum 0x5bf = GDK_KEY_Arabic_question_mark
  toEnum 0x5c1 = GDK_KEY_Arabic_hamza
  toEnum 0x5c2 = GDK_KEY_Arabic_maddaonalef
  toEnum 0x5c3 = GDK_KEY_Arabic_hamzaonalef
  toEnum 0x5c4 = GDK_KEY_Arabic_hamzaonwaw
  toEnum 0x5c5 = GDK_KEY_Arabic_hamzaunderalef
  toEnum 0x5c6 = GDK_KEY_Arabic_hamzaonyeh
  toEnum 0x5c7 = GDK_KEY_Arabic_alef
  toEnum 0x5c8 = GDK_KEY_Arabic_beh
  toEnum 0x5c9 = GDK_KEY_Arabic_tehmarbuta
  toEnum 0x5ca = GDK_KEY_Arabic_teh
  toEnum 0x5cb = GDK_KEY_Arabic_theh
  toEnum 0x5cc = GDK_KEY_Arabic_jeem
  toEnum 0x5cd = GDK_KEY_Arabic_hah
  toEnum 0x5ce = GDK_KEY_Arabic_khah
  toEnum 0x5cf = GDK_KEY_Arabic_dal
  toEnum 0x5d0 = GDK_KEY_Arabic_thal
  toEnum 0x5d1 = GDK_KEY_Arabic_ra
  toEnum 0x5d2 = GDK_KEY_Arabic_zain
  toEnum 0x5d3 = GDK_KEY_Arabic_seen
  toEnum 0x5d4 = GDK_KEY_Arabic_sheen
  toEnum 0x5d5 = GDK_KEY_Arabic_sad
  toEnum 0x5d6 = GDK_KEY_Arabic_dad
  toEnum 0x5d7 = GDK_KEY_Arabic_tah
  toEnum 0x5d8 = GDK_KEY_Arabic_zah
  toEnum 0x5d9 = GDK_KEY_Arabic_ain
  toEnum 0x5da = GDK_KEY_Arabic_ghain
  toEnum 0x5e0 = GDK_KEY_Arabic_tatweel
  toEnum 0x5e1 = GDK_KEY_Arabic_feh
  toEnum 0x5e2 = GDK_KEY_Arabic_qaf
  toEnum 0x5e3 = GDK_KEY_Arabic_kaf
  toEnum 0x5e4 = GDK_KEY_Arabic_lam
  toEnum 0x5e5 = GDK_KEY_Arabic_meem
  toEnum 0x5e6 = GDK_KEY_Arabic_noon
  toEnum 0x5e7 = GDK_KEY_Arabic_ha
  toEnum 0x5e8 = GDK_KEY_Arabic_waw
  toEnum 0x5e9 = GDK_KEY_Arabic_alefmaksura
  toEnum 0x5ea = GDK_KEY_Arabic_yeh
  toEnum 0x5eb = GDK_KEY_Arabic_fathatan
  toEnum 0x5ec = GDK_KEY_Arabic_dammatan
  toEnum 0x5ed = GDK_KEY_Arabic_kasratan
  toEnum 0x5ee = GDK_KEY_Arabic_fatha
  toEnum 0x5ef = GDK_KEY_Arabic_damma
  toEnum 0x5f0 = GDK_KEY_Arabic_kasra
  toEnum 0x5f1 = GDK_KEY_Arabic_shadda
  toEnum 0x5f2 = GDK_KEY_Arabic_sukun
  toEnum 0x6a1 = GDK_KEY_Serbian_dje
  toEnum 0x6a2 = GDK_KEY_Macedonia_gje
  toEnum 0x6a3 = GDK_KEY_Cyrillic_io
  toEnum 0x6a4 = GDK_KEY_Ukranian_je
  toEnum 0x6a5 = GDK_KEY_Macedonia_dse
  toEnum 0x6a6 = GDK_KEY_Ukrainian_i
  toEnum 0x6a7 = GDK_KEY_Ukrainian_yi
  toEnum 0x6a8 = GDK_KEY_Cyrillic_je
  toEnum 0x6a9 = GDK_KEY_Cyrillic_lje
  toEnum 0x6aa = GDK_KEY_Cyrillic_nje
  toEnum 0x6ab = GDK_KEY_Serbian_tshe
  toEnum 0x6ac = GDK_KEY_Macedonia_kje
  toEnum 0x6ad = GDK_KEY_Ukrainian_ghe_with_upturn
  toEnum 0x6ae = GDK_KEY_Byelorussian_shortu
  toEnum 0x6af = GDK_KEY_Cyrillic_dzhe
  toEnum 0x6b0 = GDK_KEY_numerosign
  toEnum 0x6b1 = GDK_KEY_Serbian_DJE
  toEnum 0x6b2 = GDK_KEY_Macedonia_GJE
  toEnum 0x6b3 = GDK_KEY_Cyrillic_IO
  toEnum 0x6b4 = GDK_KEY_Ukrainian_IE
  toEnum 0x6b5 = GDK_KEY_Macedonia_DSE
  toEnum 0x6b6 = GDK_KEY_Ukrainian_I
  toEnum 0x6b7 = GDK_KEY_Ukrainian_YI
  toEnum 0x6b8 = GDK_KEY_Cyrillic_JE
  toEnum 0x6b9 = GDK_KEY_Cyrillic_LJE
  toEnum 0x6ba = GDK_KEY_Cyrillic_NJE
  toEnum 0x6bb = GDK_KEY_Serbian_TSHE
  toEnum 0x6bc = GDK_KEY_Macedonia_KJE
  toEnum 0x6bd = GDK_KEY_Ukrainian_GHE_WITH_UPTURN
  toEnum 0x6be = GDK_KEY_Byelorussian_SHORTU
  toEnum 0x6bf = GDK_KEY_Cyrillic_DZHE
  toEnum 0x6c0 = GDK_KEY_Cyrillic_yu
  toEnum 0x6c1 = GDK_KEY_Cyrillic_a
  toEnum 0x6c2 = GDK_KEY_Cyrillic_be
  toEnum 0x6c3 = GDK_KEY_Cyrillic_tse
  toEnum 0x6c4 = GDK_KEY_Cyrillic_de
  toEnum 0x6c5 = GDK_KEY_Cyrillic_ie
  toEnum 0x6c6 = GDK_KEY_Cyrillic_ef
  toEnum 0x6c7 = GDK_KEY_Cyrillic_ghe
  toEnum 0x6c8 = GDK_KEY_Cyrillic_ha
  toEnum 0x6c9 = GDK_KEY_Cyrillic_i
  toEnum 0x6ca = GDK_KEY_Cyrillic_shorti
  toEnum 0x6cb = GDK_KEY_Cyrillic_ka
  toEnum 0x6cc = GDK_KEY_Cyrillic_el
  toEnum 0x6cd = GDK_KEY_Cyrillic_em
  toEnum 0x6ce = GDK_KEY_Cyrillic_en
  toEnum 0x6cf = GDK_KEY_Cyrillic_o
  toEnum 0x6d0 = GDK_KEY_Cyrillic_pe
  toEnum 0x6d1 = GDK_KEY_Cyrillic_ya
  toEnum 0x6d2 = GDK_KEY_Cyrillic_er
  toEnum 0x6d3 = GDK_KEY_Cyrillic_es
  toEnum 0x6d4 = GDK_KEY_Cyrillic_te
  toEnum 0x6d5 = GDK_KEY_Cyrillic_u
  toEnum 0x6d6 = GDK_KEY_Cyrillic_zhe
  toEnum 0x6d7 = GDK_KEY_Cyrillic_ve
  toEnum 0x6d8 = GDK_KEY_Cyrillic_softsign
  toEnum 0x6d9 = GDK_KEY_Cyrillic_yeru
  toEnum 0x6da = GDK_KEY_Cyrillic_ze
  toEnum 0x6db = GDK_KEY_Cyrillic_sha
  toEnum 0x6dc = GDK_KEY_Cyrillic_e
  toEnum 0x6dd = GDK_KEY_Cyrillic_shcha
  toEnum 0x6de = GDK_KEY_Cyrillic_che
  toEnum 0x6df = GDK_KEY_Cyrillic_hardsign
  toEnum 0x6e0 = GDK_KEY_Cyrillic_YU
  toEnum 0x6e1 = GDK_KEY_Cyrillic_A
  toEnum 0x6e2 = GDK_KEY_Cyrillic_BE
  toEnum 0x6e3 = GDK_KEY_Cyrillic_TSE
  toEnum 0x6e4 = GDK_KEY_Cyrillic_DE
  toEnum 0x6e5 = GDK_KEY_Cyrillic_IE
  toEnum 0x6e6 = GDK_KEY_Cyrillic_EF
  toEnum 0x6e7 = GDK_KEY_Cyrillic_GHE
  toEnum 0x6e8 = GDK_KEY_Cyrillic_HA
  toEnum 0x6e9 = GDK_KEY_Cyrillic_I
  toEnum 0x6ea = GDK_KEY_Cyrillic_SHORTI
  toEnum 0x6eb = GDK_KEY_Cyrillic_KA
  toEnum 0x6ec = GDK_KEY_Cyrillic_EL
  toEnum 0x6ed = GDK_KEY_Cyrillic_EM
  toEnum 0x6ee = GDK_KEY_Cyrillic_EN
  toEnum 0x6ef = GDK_KEY_Cyrillic_O
  toEnum 0x6f0 = GDK_KEY_Cyrillic_PE
  toEnum 0x6f1 = GDK_KEY_Cyrillic_YA
  toEnum 0x6f2 = GDK_KEY_Cyrillic_ER
  toEnum 0x6f3 = GDK_KEY_Cyrillic_ES
  toEnum 0x6f4 = GDK_KEY_Cyrillic_TE
  toEnum 0x6f5 = GDK_KEY_Cyrillic_U
  toEnum 0x6f6 = GDK_KEY_Cyrillic_ZHE
  toEnum 0x6f7 = GDK_KEY_Cyrillic_VE
  toEnum 0x6f8 = GDK_KEY_Cyrillic_SOFTSIGN
  toEnum 0x6f9 = GDK_KEY_Cyrillic_YERU
  toEnum 0x6fa = GDK_KEY_Cyrillic_ZE
  toEnum 0x6fb = GDK_KEY_Cyrillic_SHA
  toEnum 0x6fc = GDK_KEY_Cyrillic_E
  toEnum 0x6fd = GDK_KEY_Cyrillic_SHCHA
  toEnum 0x6fe = GDK_KEY_Cyrillic_CHE
  toEnum 0x6ff = GDK_KEY_Cyrillic_HARDSIGN
  toEnum 0x7a1 = GDK_KEY_Greek_ALPHAaccent
  toEnum 0x7a2 = GDK_KEY_Greek_EPSILONaccent
  toEnum 0x7a3 = GDK_KEY_Greek_ETAaccent
  toEnum 0x7a4 = GDK_KEY_Greek_IOTAaccent
  toEnum 0x7a5 = GDK_KEY_Greek_IOTAdiaeresis
  toEnum 0x7a7 = GDK_KEY_Greek_OMICRONaccent
  toEnum 0x7a8 = GDK_KEY_Greek_UPSILONaccent
  toEnum 0x7a9 = GDK_KEY_Greek_UPSILONdieresis
  toEnum 0x7ab = GDK_KEY_Greek_OMEGAaccent
  toEnum 0x7ae = GDK_KEY_Greek_accentdieresis
  toEnum 0x7af = GDK_KEY_Greek_horizbar
  toEnum 0x7b1 = GDK_KEY_Greek_alphaaccent
  toEnum 0x7b2 = GDK_KEY_Greek_epsilonaccent
  toEnum 0x7b3 = GDK_KEY_Greek_etaaccent
  toEnum 0x7b4 = GDK_KEY_Greek_iotaaccent
  toEnum 0x7b5 = GDK_KEY_Greek_iotadieresis
  toEnum 0x7b6 = GDK_KEY_Greek_iotaaccentdieresis
  toEnum 0x7b7 = GDK_KEY_Greek_omicronaccent
  toEnum 0x7b8 = GDK_KEY_Greek_upsilonaccent
  toEnum 0x7b9 = GDK_KEY_Greek_upsilondieresis
  toEnum 0x7ba = GDK_KEY_Greek_upsilonaccentdieresis
  toEnum 0x7bb = GDK_KEY_Greek_omegaaccent
  toEnum 0x7c1 = GDK_KEY_Greek_ALPHA
  toEnum 0x7c2 = GDK_KEY_Greek_BETA
  toEnum 0x7c3 = GDK_KEY_Greek_GAMMA
  toEnum 0x7c4 = GDK_KEY_Greek_DELTA
  toEnum 0x7c5 = GDK_KEY_Greek_EPSILON
  toEnum 0x7c6 = GDK_KEY_Greek_ZETA
  toEnum 0x7c7 = GDK_KEY_Greek_ETA
  toEnum 0x7c8 = GDK_KEY_Greek_THETA
  toEnum 0x7c9 = GDK_KEY_Greek_IOTA
  toEnum 0x7ca = GDK_KEY_Greek_KAPPA
  toEnum 0x7cb = GDK_KEY_Greek_LAMBDA
  toEnum 0x7cc = GDK_KEY_Greek_MU
  toEnum 0x7cd = GDK_KEY_Greek_NU
  toEnum 0x7ce = GDK_KEY_Greek_XI
  toEnum 0x7cf = GDK_KEY_Greek_OMICRON
  toEnum 0x7d0 = GDK_KEY_Greek_PI
  toEnum 0x7d1 = GDK_KEY_Greek_RHO
  toEnum 0x7d2 = GDK_KEY_Greek_SIGMA
  toEnum 0x7d4 = GDK_KEY_Greek_TAU
  toEnum 0x7d5 = GDK_KEY_Greek_UPSILON
  toEnum 0x7d6 = GDK_KEY_Greek_PHI
  toEnum 0x7d7 = GDK_KEY_Greek_CHI
  toEnum 0x7d8 = GDK_KEY_Greek_PSI
  toEnum 0x7d9 = GDK_KEY_Greek_OMEGA
  toEnum 0x7e1 = GDK_KEY_Greek_alpha
  toEnum 0x7e2 = GDK_KEY_Greek_beta
  toEnum 0x7e3 = GDK_KEY_Greek_gamma
  toEnum 0x7e4 = GDK_KEY_Greek_delta
  toEnum 0x7e5 = GDK_KEY_Greek_epsilon
  toEnum 0x7e6 = GDK_KEY_Greek_zeta
  toEnum 0x7e7 = GDK_KEY_Greek_eta
  toEnum 0x7e8 = GDK_KEY_Greek_theta
  toEnum 0x7e9 = GDK_KEY_Greek_iota
  toEnum 0x7ea = GDK_KEY_Greek_kappa
  toEnum 0x7eb = GDK_KEY_Greek_lambda
  toEnum 0x7ec = GDK_KEY_Greek_mu
  toEnum 0x7ed = GDK_KEY_Greek_nu
  toEnum 0x7ee = GDK_KEY_Greek_xi
  toEnum 0x7ef = GDK_KEY_Greek_omicron
  toEnum 0x7f0 = GDK_KEY_Greek_pi
  toEnum 0x7f1 = GDK_KEY_Greek_rho
  toEnum 0x7f2 = GDK_KEY_Greek_sigma
  toEnum 0x7f3 = GDK_KEY_Greek_finalsmallsigma
  toEnum 0x7f4 = GDK_KEY_Greek_tau
  toEnum 0x7f5 = GDK_KEY_Greek_upsilon
  toEnum 0x7f6 = GDK_KEY_Greek_phi
  toEnum 0x7f7 = GDK_KEY_Greek_chi
  toEnum 0x7f8 = GDK_KEY_Greek_psi
  toEnum 0x7f9 = GDK_KEY_Greek_omega
  toEnum 0x8a1 = GDK_KEY_leftradical
  toEnum 0x8a2 = GDK_KEY_topleftradical
  toEnum 0x8a3 = GDK_KEY_horizconnector
  toEnum 0x8a4 = GDK_KEY_topintegral
  toEnum 0x8a5 = GDK_KEY_botintegral
  toEnum 0x8a6 = GDK_KEY_vertconnector
  toEnum 0x8a7 = GDK_KEY_topleftsqbracket
  toEnum 0x8a8 = GDK_KEY_botleftsqbracket
  toEnum 0x8a9 = GDK_KEY_toprightsqbracket
  toEnum 0x8aa = GDK_KEY_botrightsqbracket
  toEnum 0x8ab = GDK_KEY_topleftparens
  toEnum 0x8ac = GDK_KEY_botleftparens
  toEnum 0x8ad = GDK_KEY_toprightparens
  toEnum 0x8ae = GDK_KEY_botrightparens
  toEnum 0x8af = GDK_KEY_leftmiddlecurlybrace
  toEnum 0x8b0 = GDK_KEY_rightmiddlecurlybrace
  toEnum 0x8b1 = GDK_KEY_topleftsummation
  toEnum 0x8b2 = GDK_KEY_botleftsummation
  toEnum 0x8b3 = GDK_KEY_topvertsummationconnector
  toEnum 0x8b4 = GDK_KEY_botvertsummationconnector
  toEnum 0x8b5 = GDK_KEY_toprightsummation
  toEnum 0x8b6 = GDK_KEY_botrightsummation
  toEnum 0x8b7 = GDK_KEY_rightmiddlesummation
  toEnum 0x8bc = GDK_KEY_lessthanequal
  toEnum 0x8bd = GDK_KEY_notequal
  toEnum 0x8be = GDK_KEY_greaterthanequal
  toEnum 0x8bf = GDK_KEY_integral
  toEnum 0x8c0 = GDK_KEY_therefore
  toEnum 0x8c1 = GDK_KEY_variation
  toEnum 0x8c2 = GDK_KEY_infinity
  toEnum 0x8c5 = GDK_KEY_nabla
  toEnum 0x8c8 = GDK_KEY_approximate
  toEnum 0x8c9 = GDK_KEY_similarequal
  toEnum 0x8cd = GDK_KEY_ifonlyif
  toEnum 0x8ce = GDK_KEY_implies
  toEnum 0x8cf = GDK_KEY_identical
  toEnum 0x8d6 = GDK_KEY_radical
  toEnum 0x8da = GDK_KEY_includedin
  toEnum 0x8db = GDK_KEY_includes
  toEnum 0x8dc = GDK_KEY_intersection
  toEnum 0x8dd = GDK_KEY_union
  toEnum 0x8de = GDK_KEY_logicaland
  toEnum 0x8df = GDK_KEY_logicalor
  toEnum 0x8ef = GDK_KEY_partialderivative
  toEnum 0x8f6 = GDK_KEY_function
  toEnum 0x8fb = GDK_KEY_leftarrow
  toEnum 0x8fc = GDK_KEY_uparrow
  toEnum 0x8fd = GDK_KEY_rightarrow
  toEnum 0x8fe = GDK_KEY_downarrow
  toEnum 0x9df = GDK_KEY_blank
  toEnum 0x9e0 = GDK_KEY_soliddiamond
  toEnum 0x9e1 = GDK_KEY_checkerboard
  toEnum 0x9e2 = GDK_KEY_ht
  toEnum 0x9e3 = GDK_KEY_ff
  toEnum 0x9e4 = GDK_KEY_cr
  toEnum 0x9e5 = GDK_KEY_lf
  toEnum 0x9e8 = GDK_KEY_nl
  toEnum 0x9e9 = GDK_KEY_vt
  toEnum 0x9ea = GDK_KEY_lowrightcorner
  toEnum 0x9eb = GDK_KEY_uprightcorner
  toEnum 0x9ec = GDK_KEY_upleftcorner
  toEnum 0x9ed = GDK_KEY_lowleftcorner
  toEnum 0x9ee = GDK_KEY_crossinglines
  toEnum 0x9ef = GDK_KEY_horizlinescan1
  toEnum 0x9f0 = GDK_KEY_horizlinescan3
  toEnum 0x9f1 = GDK_KEY_horizlinescan5
  toEnum 0x9f2 = GDK_KEY_horizlinescan7
  toEnum 0x9f3 = GDK_KEY_horizlinescan9
  toEnum 0x9f4 = GDK_KEY_leftt
  toEnum 0x9f5 = GDK_KEY_rightt
  toEnum 0x9f6 = GDK_KEY_bott
  toEnum 0x9f7 = GDK_KEY_topt
  toEnum 0x9f8 = GDK_KEY_vertbar
  toEnum 0xaa1 = GDK_KEY_emspace
  toEnum 0xaa2 = GDK_KEY_enspace
  toEnum 0xaa3 = GDK_KEY_em3space
  toEnum 0xaa4 = GDK_KEY_em4space
  toEnum 0xaa5 = GDK_KEY_digitspace
  toEnum 0xaa6 = GDK_KEY_punctspace
  toEnum 0xaa7 = GDK_KEY_thinspace
  toEnum 0xaa8 = GDK_KEY_hairspace
  toEnum 0xaa9 = GDK_KEY_emdash
  toEnum 0xaaa = GDK_KEY_endash
  toEnum 0xaac = GDK_KEY_signifblank
  toEnum 0xaae = GDK_KEY_ellipsis
  toEnum 0xaaf = GDK_KEY_doubbaselinedot
  toEnum 0xab0 = GDK_KEY_onethird
  toEnum 0xab1 = GDK_KEY_twothirds
  toEnum 0xab2 = GDK_KEY_onefifth
  toEnum 0xab3 = GDK_KEY_twofifths
  toEnum 0xab4 = GDK_KEY_threefifths
  toEnum 0xab5 = GDK_KEY_fourfifths
  toEnum 0xab6 = GDK_KEY_onesixth
  toEnum 0xab7 = GDK_KEY_fivesixths
  toEnum 0xab8 = GDK_KEY_careof
  toEnum 0xabb = GDK_KEY_figdash
  toEnum 0xabc = GDK_KEY_leftanglebracket
  toEnum 0xabd = GDK_KEY_decimalpoint
  toEnum 0xabe = GDK_KEY_rightanglebracket
  toEnum 0xabf = GDK_KEY_marker
  toEnum 0xac3 = GDK_KEY_oneeighth
  toEnum 0xac4 = GDK_KEY_threeeighths
  toEnum 0xac5 = GDK_KEY_fiveeighths
  toEnum 0xac6 = GDK_KEY_seveneighths
  toEnum 0xac9 = GDK_KEY_trademark
  toEnum 0xaca = GDK_KEY_signaturemark
  toEnum 0xacb = GDK_KEY_trademarkincircle
  toEnum 0xacc = GDK_KEY_leftopentriangle
  toEnum 0xacd = GDK_KEY_rightopentriangle
  toEnum 0xace = GDK_KEY_emopencircle
  toEnum 0xacf = GDK_KEY_emopenrectangle
  toEnum 0xad0 = GDK_KEY_leftsinglequotemark
  toEnum 0xad1 = GDK_KEY_rightsinglequotemark
  toEnum 0xad2 = GDK_KEY_leftdoublequotemark
  toEnum 0xad3 = GDK_KEY_rightdoublequotemark
  toEnum 0xad4 = GDK_KEY_prescription
  toEnum 0xad5 = GDK_KEY_permille
  toEnum 0xad6 = GDK_KEY_minutes
  toEnum 0xad7 = GDK_KEY_seconds
  toEnum 0xad9 = GDK_KEY_latincross
  toEnum 0xada = GDK_KEY_hexagram
  toEnum 0xadb = GDK_KEY_filledrectbullet
  toEnum 0xadc = GDK_KEY_filledlefttribullet
  toEnum 0xadd = GDK_KEY_filledrighttribullet
  toEnum 0xade = GDK_KEY_emfilledcircle
  toEnum 0xadf = GDK_KEY_emfilledrect
  toEnum 0xae0 = GDK_KEY_enopencircbullet
  toEnum 0xae1 = GDK_KEY_enopensquarebullet
  toEnum 0xae2 = GDK_KEY_openrectbullet
  toEnum 0xae3 = GDK_KEY_opentribulletup
  toEnum 0xae4 = GDK_KEY_opentribulletdown
  toEnum 0xae5 = GDK_KEY_openstar
  toEnum 0xae6 = GDK_KEY_enfilledcircbullet
  toEnum 0xae7 = GDK_KEY_enfilledsqbullet
  toEnum 0xae8 = GDK_KEY_filledtribulletup
  toEnum 0xae9 = GDK_KEY_filledtribulletdown
  toEnum 0xaea = GDK_KEY_leftpointer
  toEnum 0xaeb = GDK_KEY_rightpointer
  toEnum 0xaec = GDK_KEY_club
  toEnum 0xaed = GDK_KEY_diamond
  toEnum 0xaee = GDK_KEY_heart
  toEnum 0xaf0 = GDK_KEY_maltesecross
  toEnum 0xaf1 = GDK_KEY_dagger
  toEnum 0xaf2 = GDK_KEY_doubledagger
  toEnum 0xaf3 = GDK_KEY_checkmark
  toEnum 0xaf4 = GDK_KEY_ballotcross
  toEnum 0xaf5 = GDK_KEY_musicalsharp
  toEnum 0xaf6 = GDK_KEY_musicalflat
  toEnum 0xaf7 = GDK_KEY_malesymbol
  toEnum 0xaf8 = GDK_KEY_femalesymbol
  toEnum 0xaf9 = GDK_KEY_telephone
  toEnum 0xafa = GDK_KEY_telephonerecorder
  toEnum 0xafb = GDK_KEY_phonographcopyright
  toEnum 0xafc = GDK_KEY_caret
  toEnum 0xafd = GDK_KEY_singlelowquotemark
  toEnum 0xafe = GDK_KEY_doublelowquotemark
  toEnum 0xaff = GDK_KEY_cursor
  toEnum 0xba3 = GDK_KEY_leftcaret
  toEnum 0xba6 = GDK_KEY_rightcaret
  toEnum 0xba8 = GDK_KEY_downcaret
  toEnum 0xba9 = GDK_KEY_upcaret
  toEnum 0xbc0 = GDK_KEY_overbar
  toEnum 0xbc2 = GDK_KEY_downtack
  toEnum 0xbc3 = GDK_KEY_upshoe
  toEnum 0xbc4 = GDK_KEY_downstile
  toEnum 0xbc6 = GDK_KEY_underbar
  toEnum 0xbca = GDK_KEY_jot
  toEnum 0xbcc = GDK_KEY_quad
  toEnum 0xbce = GDK_KEY_uptack
  toEnum 0xbcf = GDK_KEY_circle
  toEnum 0xbd3 = GDK_KEY_upstile
  toEnum 0xbd6 = GDK_KEY_downshoe
  toEnum 0xbd8 = GDK_KEY_rightshoe
  toEnum 0xbda = GDK_KEY_leftshoe
  toEnum 0xbdc = GDK_KEY_lefttack
  toEnum 0xbfc = GDK_KEY_righttack
  toEnum 0xcdf = GDK_KEY_hebrew_doublelowline
  toEnum 0xce0 = GDK_KEY_hebrew_aleph
  toEnum 0xce1 = GDK_KEY_hebrew_beth
  toEnum 0xce2 = GDK_KEY_hebrew_gimmel
  toEnum 0xce3 = GDK_KEY_hebrew_daleth
  toEnum 0xce4 = GDK_KEY_hebrew_he
  toEnum 0xce5 = GDK_KEY_hebrew_waw
  toEnum 0xce6 = GDK_KEY_hebrew_zayin
  toEnum 0xce7 = GDK_KEY_hebrew_het
  toEnum 0xce8 = GDK_KEY_hebrew_teth
  toEnum 0xce9 = GDK_KEY_hebrew_yod
  toEnum 0xcea = GDK_KEY_hebrew_finalkaph
  toEnum 0xceb = GDK_KEY_hebrew_kaph
  toEnum 0xcec = GDK_KEY_hebrew_lamed
  toEnum 0xced = GDK_KEY_hebrew_finalmem
  toEnum 0xcee = GDK_KEY_hebrew_mem
  toEnum 0xcef = GDK_KEY_hebrew_finalnun
  toEnum 0xcf0 = GDK_KEY_hebrew_nun
  toEnum 0xcf1 = GDK_KEY_hebrew_samekh
  toEnum 0xcf2 = GDK_KEY_hebrew_ayin
  toEnum 0xcf3 = GDK_KEY_hebrew_finalpe
  toEnum 0xcf4 = GDK_KEY_hebrew_pe
  toEnum 0xcf5 = GDK_KEY_hebrew_finalzadi
  toEnum 0xcf6 = GDK_KEY_hebrew_zadi
  toEnum 0xcf7 = GDK_KEY_hebrew_qoph
  toEnum 0xcf8 = GDK_KEY_hebrew_resh
  toEnum 0xcf9 = GDK_KEY_hebrew_shin
  toEnum 0xcfa = GDK_KEY_hebrew_taf
  toEnum 0xda1 = GDK_KEY_Thai_kokai
  toEnum 0xda2 = GDK_KEY_Thai_khokhai
  toEnum 0xda3 = GDK_KEY_Thai_khokhuat
  toEnum 0xda4 = GDK_KEY_Thai_khokhwai
  toEnum 0xda5 = GDK_KEY_Thai_khokhon
  toEnum 0xda6 = GDK_KEY_Thai_khorakhang
  toEnum 0xda7 = GDK_KEY_Thai_ngongu
  toEnum 0xda8 = GDK_KEY_Thai_chochan
  toEnum 0xda9 = GDK_KEY_Thai_choching
  toEnum 0xdaa = GDK_KEY_Thai_chochang
  toEnum 0xdab = GDK_KEY_Thai_soso
  toEnum 0xdac = GDK_KEY_Thai_chochoe
  toEnum 0xdad = GDK_KEY_Thai_yoying
  toEnum 0xdae = GDK_KEY_Thai_dochada
  toEnum 0xdaf = GDK_KEY_Thai_topatak
  toEnum 0xdb0 = GDK_KEY_Thai_thothan
  toEnum 0xdb1 = GDK_KEY_Thai_thonangmontho
  toEnum 0xdb2 = GDK_KEY_Thai_thophuthao
  toEnum 0xdb3 = GDK_KEY_Thai_nonen
  toEnum 0xdb4 = GDK_KEY_Thai_dodek
  toEnum 0xdb5 = GDK_KEY_Thai_totao
  toEnum 0xdb6 = GDK_KEY_Thai_thothung
  toEnum 0xdb7 = GDK_KEY_Thai_thothahan
  toEnum 0xdb8 = GDK_KEY_Thai_thothong
  toEnum 0xdb9 = GDK_KEY_Thai_nonu
  toEnum 0xdba = GDK_KEY_Thai_bobaimai
  toEnum 0xdbb = GDK_KEY_Thai_popla
  toEnum 0xdbc = GDK_KEY_Thai_phophung
  toEnum 0xdbd = GDK_KEY_Thai_fofa
  toEnum 0xdbe = GDK_KEY_Thai_phophan
  toEnum 0xdbf = GDK_KEY_Thai_fofan
  toEnum 0xdc0 = GDK_KEY_Thai_phosamphao
  toEnum 0xdc1 = GDK_KEY_Thai_moma
  toEnum 0xdc2 = GDK_KEY_Thai_yoyak
  toEnum 0xdc3 = GDK_KEY_Thai_rorua
  toEnum 0xdc4 = GDK_KEY_Thai_ru
  toEnum 0xdc5 = GDK_KEY_Thai_loling
  toEnum 0xdc6 = GDK_KEY_Thai_lu
  toEnum 0xdc7 = GDK_KEY_Thai_wowaen
  toEnum 0xdc8 = GDK_KEY_Thai_sosala
  toEnum 0xdc9 = GDK_KEY_Thai_sorusi
  toEnum 0xdca = GDK_KEY_Thai_sosua
  toEnum 0xdcb = GDK_KEY_Thai_hohip
  toEnum 0xdcc = GDK_KEY_Thai_lochula
  toEnum 0xdcd = GDK_KEY_Thai_oang
  toEnum 0xdce = GDK_KEY_Thai_honokhuk
  toEnum 0xdcf = GDK_KEY_Thai_paiyannoi
  toEnum 0xdd0 = GDK_KEY_Thai_saraa
  toEnum 0xdd1 = GDK_KEY_Thai_maihanakat
  toEnum 0xdd2 = GDK_KEY_Thai_saraaa
  toEnum 0xdd3 = GDK_KEY_Thai_saraam
  toEnum 0xdd4 = GDK_KEY_Thai_sarai
  toEnum 0xdd5 = GDK_KEY_Thai_saraii
  toEnum 0xdd6 = GDK_KEY_Thai_saraue
  toEnum 0xdd7 = GDK_KEY_Thai_sarauee
  toEnum 0xdd8 = GDK_KEY_Thai_sarau
  toEnum 0xdd9 = GDK_KEY_Thai_sarauu
  toEnum 0xdda = GDK_KEY_Thai_phinthu
  toEnum 0xdde = GDK_KEY_Thai_maihanakat_maitho
  toEnum 0xddf = GDK_KEY_Thai_baht
  toEnum 0xde0 = GDK_KEY_Thai_sarae
  toEnum 0xde1 = GDK_KEY_Thai_saraae
  toEnum 0xde2 = GDK_KEY_Thai_sarao
  toEnum 0xde3 = GDK_KEY_Thai_saraaimaimuan
  toEnum 0xde4 = GDK_KEY_Thai_saraaimaimalai
  toEnum 0xde5 = GDK_KEY_Thai_lakkhangyao
  toEnum 0xde6 = GDK_KEY_Thai_maiyamok
  toEnum 0xde7 = GDK_KEY_Thai_maitaikhu
  toEnum 0xde8 = GDK_KEY_Thai_maiek
  toEnum 0xde9 = GDK_KEY_Thai_maitho
  toEnum 0xdea = GDK_KEY_Thai_maitri
  toEnum 0xdeb = GDK_KEY_Thai_maichattawa
  toEnum 0xdec = GDK_KEY_Thai_thanthakhat
  toEnum 0xded = GDK_KEY_Thai_nikhahit
  toEnum 0xdf0 = GDK_KEY_Thai_leksun
  toEnum 0xdf1 = GDK_KEY_Thai_leknung
  toEnum 0xdf2 = GDK_KEY_Thai_leksong
  toEnum 0xdf3 = GDK_KEY_Thai_leksam
  toEnum 0xdf4 = GDK_KEY_Thai_leksi
  toEnum 0xdf5 = GDK_KEY_Thai_lekha
  toEnum 0xdf6 = GDK_KEY_Thai_lekhok
  toEnum 0xdf7 = GDK_KEY_Thai_lekchet
  toEnum 0xdf8 = GDK_KEY_Thai_lekpaet
  toEnum 0xdf9 = GDK_KEY_Thai_lekkao
  toEnum 0xea1 = GDK_KEY_Hangul_Kiyeog
  toEnum 0xea2 = GDK_KEY_Hangul_SsangKiyeog
  toEnum 0xea3 = GDK_KEY_Hangul_KiyeogSios
  toEnum 0xea4 = GDK_KEY_Hangul_Nieun
  toEnum 0xea5 = GDK_KEY_Hangul_NieunJieuj
  toEnum 0xea6 = GDK_KEY_Hangul_NieunHieuh
  toEnum 0xea7 = GDK_KEY_Hangul_Dikeud
  toEnum 0xea8 = GDK_KEY_Hangul_SsangDikeud
  toEnum 0xea9 = GDK_KEY_Hangul_Rieul
  toEnum 0xeaa = GDK_KEY_Hangul_RieulKiyeog
  toEnum 0xeab = GDK_KEY_Hangul_RieulMieum
  toEnum 0xeac = GDK_KEY_Hangul_RieulPieub
  toEnum 0xead = GDK_KEY_Hangul_RieulSios
  toEnum 0xeae = GDK_KEY_Hangul_RieulTieut
  toEnum 0xeaf = GDK_KEY_Hangul_RieulPhieuf
  toEnum 0xeb0 = GDK_KEY_Hangul_RieulHieuh
  toEnum 0xeb1 = GDK_KEY_Hangul_Mieum
  toEnum 0xeb2 = GDK_KEY_Hangul_Pieub
  toEnum 0xeb3 = GDK_KEY_Hangul_SsangPieub
  toEnum 0xeb4 = GDK_KEY_Hangul_PieubSios
  toEnum 0xeb5 = GDK_KEY_Hangul_Sios
  toEnum 0xeb6 = GDK_KEY_Hangul_SsangSios
  toEnum 0xeb7 = GDK_KEY_Hangul_Ieung
  toEnum 0xeb8 = GDK_KEY_Hangul_Jieuj
  toEnum 0xeb9 = GDK_KEY_Hangul_SsangJieuj
  toEnum 0xeba = GDK_KEY_Hangul_Cieuc
  toEnum 0xebb = GDK_KEY_Hangul_Khieuq
  toEnum 0xebc = GDK_KEY_Hangul_Tieut
  toEnum 0xebd = GDK_KEY_Hangul_Phieuf
  toEnum 0xebe = GDK_KEY_Hangul_Hieuh
  toEnum 0xebf = GDK_KEY_Hangul_A
  toEnum 0xec0 = GDK_KEY_Hangul_AE
  toEnum 0xec1 = GDK_KEY_Hangul_YA
  toEnum 0xec2 = GDK_KEY_Hangul_YAE
  toEnum 0xec3 = GDK_KEY_Hangul_EO
  toEnum 0xec4 = GDK_KEY_Hangul_E
  toEnum 0xec5 = GDK_KEY_Hangul_YEO
  toEnum 0xec6 = GDK_KEY_Hangul_YE
  toEnum 0xec7 = GDK_KEY_Hangul_O
  toEnum 0xec8 = GDK_KEY_Hangul_WA
  toEnum 0xec9 = GDK_KEY_Hangul_WAE
  toEnum 0xeca = GDK_KEY_Hangul_OE
  toEnum 0xecb = GDK_KEY_Hangul_YO
  toEnum 0xecc = GDK_KEY_Hangul_U
  toEnum 0xecd = GDK_KEY_Hangul_WEO
  toEnum 0xece = GDK_KEY_Hangul_WE
  toEnum 0xecf = GDK_KEY_Hangul_WI
  toEnum 0xed0 = GDK_KEY_Hangul_YU
  toEnum 0xed1 = GDK_KEY_Hangul_EU
  toEnum 0xed2 = GDK_KEY_Hangul_YI
  toEnum 0xed3 = GDK_KEY_Hangul_I
  toEnum 0xed4 = GDK_KEY_Hangul_J_Kiyeog
  toEnum 0xed5 = GDK_KEY_Hangul_J_SsangKiyeog
  toEnum 0xed6 = GDK_KEY_Hangul_J_KiyeogSios
  toEnum 0xed7 = GDK_KEY_Hangul_J_Nieun
  toEnum 0xed8 = GDK_KEY_Hangul_J_NieunJieuj
  toEnum 0xed9 = GDK_KEY_Hangul_J_NieunHieuh
  toEnum 0xeda = GDK_KEY_Hangul_J_Dikeud
  toEnum 0xedb = GDK_KEY_Hangul_J_Rieul
  toEnum 0xedc = GDK_KEY_Hangul_J_RieulKiyeog
  toEnum 0xedd = GDK_KEY_Hangul_J_RieulMieum
  toEnum 0xede = GDK_KEY_Hangul_J_RieulPieub
  toEnum 0xedf = GDK_KEY_Hangul_J_RieulSios
  toEnum 0xee0 = GDK_KEY_Hangul_J_RieulTieut
  toEnum 0xee1 = GDK_KEY_Hangul_J_RieulPhieuf
  toEnum 0xee2 = GDK_KEY_Hangul_J_RieulHieuh
  toEnum 0xee3 = GDK_KEY_Hangul_J_Mieum
  toEnum 0xee4 = GDK_KEY_Hangul_J_Pieub
  toEnum 0xee5 = GDK_KEY_Hangul_J_PieubSios
  toEnum 0xee6 = GDK_KEY_Hangul_J_Sios
  toEnum 0xee7 = GDK_KEY_Hangul_J_SsangSios
  toEnum 0xee8 = GDK_KEY_Hangul_J_Ieung
  toEnum 0xee9 = GDK_KEY_Hangul_J_Jieuj
  toEnum 0xeea = GDK_KEY_Hangul_J_Cieuc
  toEnum 0xeeb = GDK_KEY_Hangul_J_Khieuq
  toEnum 0xeec = GDK_KEY_Hangul_J_Tieut
  toEnum 0xeed = GDK_KEY_Hangul_J_Phieuf
  toEnum 0xeee = GDK_KEY_Hangul_J_Hieuh
  toEnum 0xeef = GDK_KEY_Hangul_RieulYeorinHieuh
  toEnum 0xef0 = GDK_KEY_Hangul_SunkyeongeumMieum
  toEnum 0xef1 = GDK_KEY_Hangul_SunkyeongeumPieub
  toEnum 0xef2 = GDK_KEY_Hangul_PanSios
  toEnum 0xef3 = GDK_KEY_Hangul_KkogjiDalrinIeung
  toEnum 0xef4 = GDK_KEY_Hangul_SunkyeongeumPhieuf
  toEnum 0xef5 = GDK_KEY_Hangul_YeorinHieuh
  toEnum 0xef6 = GDK_KEY_Hangul_AraeA
  toEnum 0xef7 = GDK_KEY_Hangul_AraeAE
  toEnum 0xef8 = GDK_KEY_Hangul_J_PanSios
  toEnum 0xef9 = GDK_KEY_Hangul_J_KkogjiDalrinIeung
  toEnum 0xefa = GDK_KEY_Hangul_J_YeorinHieuh
  toEnum 0xeff = GDK_KEY_Korean_Won
  toEnum 0xfd01 = GDK_KEY_3270_Duplicate
  toEnum 0xfd02 = GDK_KEY_3270_FieldMark
  toEnum 0xfd03 = GDK_KEY_3270_Right2
  toEnum 0xfd04 = GDK_KEY_3270_Left2
  toEnum 0xfd05 = GDK_KEY_3270_BackTab
  toEnum 0xfd06 = GDK_KEY_3270_EraseEOF
  toEnum 0xfd07 = GDK_KEY_3270_EraseInput
  toEnum 0xfd08 = GDK_KEY_3270_Reset
  toEnum 0xfd09 = GDK_KEY_3270_Quit
  toEnum 0xfd0a = GDK_KEY_3270_PA1
  toEnum 0xfd0b = GDK_KEY_3270_PA2
  toEnum 0xfd0c = GDK_KEY_3270_PA3
  toEnum 0xfd0d = GDK_KEY_3270_Test
  toEnum 0xfd0e = GDK_KEY_3270_Attn
  toEnum 0xfd0f = GDK_KEY_3270_CursorBlink
  toEnum 0xfd10 = GDK_KEY_3270_AltCursor
  toEnum 0xfd11 = GDK_KEY_3270_KeyClick
  toEnum 0xfd12 = GDK_KEY_3270_Jump
  toEnum 0xfd13 = GDK_KEY_3270_Ident
  toEnum 0xfd14 = GDK_KEY_3270_Rule
  toEnum 0xfd15 = GDK_KEY_3270_Copy
  toEnum 0xfd16 = GDK_KEY_3270_Play
  toEnum 0xfd17 = GDK_KEY_3270_Setup
  toEnum 0xfd18 = GDK_KEY_3270_Record
  toEnum 0xfd19 = GDK_KEY_3270_ChangeScreen
  toEnum 0xfd1a = GDK_KEY_3270_DeleteWord
  toEnum 0xfd1b = GDK_KEY_3270_ExSelect
  toEnum 0xfd1c = GDK_KEY_3270_CursorSelect
  toEnum 0xfd1d = GDK_KEY_3270_PrintScreen
  toEnum 0xfd1e = GDK_KEY_3270_Enter
  toEnum 0xfe01 = GDK_KEY_ISO_Lock
  toEnum 0xfe02 = GDK_KEY_ISO_Level2_Latch
  toEnum 0xfe03 = GDK_KEY_ISO_Level3_Shift
  toEnum 0xfe04 = GDK_KEY_ISO_Level3_Latch
  toEnum 0xfe05 = GDK_KEY_ISO_Level3_Lock
  toEnum 0xfe06 = GDK_KEY_ISO_Group_Latch
  toEnum 0xfe07 = GDK_KEY_ISO_Group_Lock
  toEnum 0xfe08 = GDK_KEY_ISO_Next_Group
  toEnum 0xfe09 = GDK_KEY_ISO_Next_Group_Lock
  toEnum 0xfe0a = GDK_KEY_ISO_Prev_Group
  toEnum 0xfe0b = GDK_KEY_ISO_Prev_Group_Lock
  toEnum 0xfe0c = GDK_KEY_ISO_First_Group
  toEnum 0xfe0d = GDK_KEY_ISO_First_Group_Lock
  toEnum 0xfe0e = GDK_KEY_ISO_Last_Group
  toEnum 0xfe0f = GDK_KEY_ISO_Last_Group_Lock
  toEnum 0xfe11 = GDK_KEY_ISO_Level5_Shift
  toEnum 0xfe12 = GDK_KEY_ISO_Level5_Latch
  toEnum 0xfe13 = GDK_KEY_ISO_Level5_Lock
  toEnum 0xfe20 = GDK_KEY_ISO_Left_Tab
  toEnum 0xfe21 = GDK_KEY_ISO_Move_Line_Up
  toEnum 0xfe22 = GDK_KEY_ISO_Move_Line_Down
  toEnum 0xfe23 = GDK_KEY_ISO_Partial_Line_Up
  toEnum 0xfe24 = GDK_KEY_ISO_Partial_Line_Down
  toEnum 0xfe25 = GDK_KEY_ISO_Partial_Space_Left
  toEnum 0xfe26 = GDK_KEY_ISO_Partial_Space_Right
  toEnum 0xfe27 = GDK_KEY_ISO_Set_Margin_Left
  toEnum 0xfe28 = GDK_KEY_ISO_Set_Margin_Right
  toEnum 0xfe29 = GDK_KEY_ISO_Release_Margin_Left
  toEnum 0xfe2a = GDK_KEY_ISO_Release_Margin_Right
  toEnum 0xfe2b = GDK_KEY_ISO_Release_Both_Margins
  toEnum 0xfe2c = GDK_KEY_ISO_Fast_Cursor_Left
  toEnum 0xfe2d = GDK_KEY_ISO_Fast_Cursor_Right
  toEnum 0xfe2e = GDK_KEY_ISO_Fast_Cursor_Up
  toEnum 0xfe2f = GDK_KEY_ISO_Fast_Cursor_Down
  toEnum 0xfe30 = GDK_KEY_ISO_Continuous_Underline
  toEnum 0xfe31 = GDK_KEY_ISO_Discontinuous_Underline
  toEnum 0xfe32 = GDK_KEY_ISO_Emphasize
  toEnum 0xfe33 = GDK_KEY_ISO_Center_Object
  toEnum 0xfe34 = GDK_KEY_ISO_Enter
  toEnum 0xfe50 = GDK_KEY_dead_grave
  toEnum 0xfe51 = GDK_KEY_dead_acute
  toEnum 0xfe52 = GDK_KEY_dead_circumflex
  toEnum 0xfe53 = GDK_KEY_dead_tilde
  toEnum 0xfe54 = GDK_KEY_dead_macron
  toEnum 0xfe55 = GDK_KEY_dead_breve
  toEnum 0xfe56 = GDK_KEY_dead_abovedot
  toEnum 0xfe57 = GDK_KEY_dead_diaeresis
  toEnum 0xfe58 = GDK_KEY_dead_abovering
  toEnum 0xfe59 = GDK_KEY_dead_doubleacute
  toEnum 0xfe5a = GDK_KEY_dead_caron
  toEnum 0xfe5b = GDK_KEY_dead_cedilla
  toEnum 0xfe5c = GDK_KEY_dead_ogonek
  toEnum 0xfe5d = GDK_KEY_dead_iota
  toEnum 0xfe5e = GDK_KEY_dead_voiced_sound
  toEnum 0xfe5f = GDK_KEY_dead_semivoiced_sound
  toEnum 0xfe60 = GDK_KEY_dead_belowdot
  toEnum 0xfe61 = GDK_KEY_dead_hook
  toEnum 0xfe62 = GDK_KEY_dead_horn
  toEnum 0xfe63 = GDK_KEY_dead_stroke
  toEnum 0xfe64 = GDK_KEY_dead_psili
  toEnum 0xfe65 = GDK_KEY_dead_dasia
  toEnum 0xfe66 = GDK_KEY_dead_doublegrave
  toEnum 0xfe67 = GDK_KEY_dead_belowring
  toEnum 0xfe68 = GDK_KEY_dead_belowmacron
  toEnum 0xfe69 = GDK_KEY_dead_belowcircumflex
  toEnum 0xfe6a = GDK_KEY_dead_belowtilde
  toEnum 0xfe6b = GDK_KEY_dead_belowbreve
  toEnum 0xfe6c = GDK_KEY_dead_belowdiaeresis
  toEnum 0xfe6d = GDK_KEY_dead_invertedbreve
  toEnum 0xfe6e = GDK_KEY_dead_belowcomma
  toEnum 0xfe6f = GDK_KEY_dead_currency
  toEnum 0xfe70 = GDK_KEY_AccessX_Enable
  toEnum 0xfe71 = GDK_KEY_AccessX_Feedback_Enable
  toEnum 0xfe72 = GDK_KEY_RepeatKeys_Enable
  toEnum 0xfe73 = GDK_KEY_SlowKeys_Enable
  toEnum 0xfe74 = GDK_KEY_BounceKeys_Enable
  toEnum 0xfe75 = GDK_KEY_StickyKeys_Enable
  toEnum 0xfe76 = GDK_KEY_MouseKeys_Enable
  toEnum 0xfe77 = GDK_KEY_MouseKeys_Accel_Enable
  toEnum 0xfe78 = GDK_KEY_Overlay1_Enable
  toEnum 0xfe79 = GDK_KEY_Overlay2_Enable
  toEnum 0xfe7a = GDK_KEY_AudibleBell_Enable
  toEnum 0xfe80 = GDK_KEY_dead_a
  toEnum 0xfe81 = GDK_KEY_dead_A
  toEnum 0xfe82 = GDK_KEY_dead_e
  toEnum 0xfe83 = GDK_KEY_dead_E
  toEnum 0xfe84 = GDK_KEY_dead_i
  toEnum 0xfe85 = GDK_KEY_dead_I
  toEnum 0xfe86 = GDK_KEY_dead_o
  toEnum 0xfe87 = GDK_KEY_dead_O
  toEnum 0xfe88 = GDK_KEY_dead_u
  toEnum 0xfe89 = GDK_KEY_dead_U
  toEnum 0xfe8a = GDK_KEY_dead_small_schwa
  toEnum 0xfe8b = GDK_KEY_dead_capital_schwa
  toEnum 0xfe8c = GDK_KEY_dead_greek
  toEnum 0xfea0 = GDK_KEY_ch
  toEnum 0xfea1 = GDK_KEY_Ch
  toEnum 0xfea2 = GDK_KEY_CH
  toEnum 0xfea3 = GDK_KEY_c_h
  toEnum 0xfea4 = GDK_KEY_C_h
  toEnum 0xfea5 = GDK_KEY_C_H
  toEnum 0xfed0 = GDK_KEY_First_Virtual_Screen
  toEnum 0xfed1 = GDK_KEY_Prev_Virtual_Screen
  toEnum 0xfed2 = GDK_KEY_Next_Virtual_Screen
  toEnum 0xfed4 = GDK_KEY_Last_Virtual_Screen
  toEnum 0xfed5 = GDK_KEY_Terminate_Server
  toEnum 0xfee0 = GDK_KEY_Pointer_Left
  toEnum 0xfee1 = GDK_KEY_Pointer_Right
  toEnum 0xfee2 = GDK_KEY_Pointer_Up
  toEnum 0xfee3 = GDK_KEY_Pointer_Down
  toEnum 0xfee4 = GDK_KEY_Pointer_UpLeft
  toEnum 0xfee5 = GDK_KEY_Pointer_UpRight
  toEnum 0xfee6 = GDK_KEY_Pointer_DownLeft
  toEnum 0xfee7 = GDK_KEY_Pointer_DownRight
  toEnum 0xfee8 = GDK_KEY_Pointer_Button_Dflt
  toEnum 0xfee9 = GDK_KEY_Pointer_Button1
  toEnum 0xfeea = GDK_KEY_Pointer_Button2
  toEnum 0xfeeb = GDK_KEY_Pointer_Button3
  toEnum 0xfeec = GDK_KEY_Pointer_Button4
  toEnum 0xfeed = GDK_KEY_Pointer_Button5
  toEnum 0xfeee = GDK_KEY_Pointer_DblClick_Dflt
  toEnum 0xfeef = GDK_KEY_Pointer_DblClick1
  toEnum 0xfef0 = GDK_KEY_Pointer_DblClick2
  toEnum 0xfef1 = GDK_KEY_Pointer_DblClick3
  toEnum 0xfef2 = GDK_KEY_Pointer_DblClick4
  toEnum 0xfef3 = GDK_KEY_Pointer_DblClick5
  toEnum 0xfef4 = GDK_KEY_Pointer_Drag_Dflt
  toEnum 0xfef5 = GDK_KEY_Pointer_Drag1
  toEnum 0xfef6 = GDK_KEY_Pointer_Drag2
  toEnum 0xfef7 = GDK_KEY_Pointer_Drag3
  toEnum 0xfef8 = GDK_KEY_Pointer_Drag4
  toEnum 0xfef9 = GDK_KEY_Pointer_EnableKeys
  toEnum 0xfefa = GDK_KEY_Pointer_Accelerate
  toEnum 0xfefb = GDK_KEY_Pointer_DfltBtnNext
  toEnum 0xfefc = GDK_KEY_Pointer_DfltBtnPrev
  toEnum 0xfefd = GDK_KEY_Pointer_Drag5
  toEnum 0xff08 = GDK_KEY_BackSpace
  toEnum 0xff09 = GDK_KEY_Tab
  toEnum 0xff0a = GDK_KEY_Linefeed
  toEnum 0xff0b = GDK_KEY_Clear
  toEnum 0xff0d = GDK_KEY_Return
  toEnum 0xff13 = GDK_KEY_Pause
  toEnum 0xff14 = GDK_KEY_Scroll_Lock
  toEnum 0xff15 = GDK_KEY_Sys_Req
  toEnum 0xff1b = GDK_KEY_Escape
  toEnum 0xff20 = GDK_KEY_Multi_key
  toEnum 0xff21 = GDK_KEY_Kanji
  toEnum 0xff22 = GDK_KEY_Muhenkan
  toEnum 0xff23 = GDK_KEY_Henkan
  toEnum 0xff24 = GDK_KEY_Romaji
  toEnum 0xff25 = GDK_KEY_Hiragana
  toEnum 0xff26 = GDK_KEY_Katakana
  toEnum 0xff27 = GDK_KEY_Hiragana_Katakana
  toEnum 0xff28 = GDK_KEY_Zenkaku
  toEnum 0xff29 = GDK_KEY_Hankaku
  toEnum 0xff2a = GDK_KEY_Zenkaku_Hankaku
  toEnum 0xff2b = GDK_KEY_Touroku
  toEnum 0xff2c = GDK_KEY_Massyo
  toEnum 0xff2d = GDK_KEY_Kana_Lock
  toEnum 0xff2e = GDK_KEY_Kana_Shift
  toEnum 0xff2f = GDK_KEY_Eisu_Shift
  toEnum 0xff30 = GDK_KEY_Eisu_toggle
  toEnum 0xff31 = GDK_KEY_Hangul
  toEnum 0xff32 = GDK_KEY_Hangul_Start
  toEnum 0xff33 = GDK_KEY_Hangul_End
  toEnum 0xff34 = GDK_KEY_Hangul_Hanja
  toEnum 0xff35 = GDK_KEY_Hangul_Jamo
  toEnum 0xff36 = GDK_KEY_Hangul_Romaja
  toEnum 0xff37 = GDK_KEY_Codeinput
  toEnum 0xff38 = GDK_KEY_Hangul_Jeonja
  toEnum 0xff39 = GDK_KEY_Hangul_Banja
  toEnum 0xff3a = GDK_KEY_Hangul_PreHanja
  toEnum 0xff3b = GDK_KEY_Hangul_PostHanja
  toEnum 0xff3c = GDK_KEY_SingleCandidate
  toEnum 0xff3d = GDK_KEY_MultipleCandidate
  toEnum 0xff3e = GDK_KEY_PreviousCandidate
  toEnum 0xff3f = GDK_KEY_Hangul_Special
  toEnum 0xff50 = GDK_KEY_Home
  toEnum 0xff51 = GDK_KEY_Left
  toEnum 0xff52 = GDK_KEY_Up
  toEnum 0xff53 = GDK_KEY_Right
  toEnum 0xff54 = GDK_KEY_Down
  toEnum 0xff55 = GDK_KEY_Page_Up
  toEnum 0xff56 = GDK_KEY_Page_Down
  toEnum 0xff57 = GDK_KEY_End
  toEnum 0xff58 = GDK_KEY_Begin
  toEnum 0xff60 = GDK_KEY_Select
  toEnum 0xff61 = GDK_KEY_Print
  toEnum 0xff62 = GDK_KEY_Execute
  toEnum 0xff63 = GDK_KEY_Insert
  toEnum 0xff65 = GDK_KEY_Undo
  toEnum 0xff66 = GDK_KEY_Redo
  toEnum 0xff67 = GDK_KEY_Menu
  toEnum 0xff68 = GDK_KEY_Find
  toEnum 0xff69 = GDK_KEY_Cancel
  toEnum 0xff6a = GDK_KEY_Help
  toEnum 0xff6b = GDK_KEY_Break
  toEnum 0xff7e = GDK_KEY_Mode_switch
  toEnum 0xff7f = GDK_KEY_Num_Lock
  toEnum 0xff80 = GDK_KEY_KP_Space
  toEnum 0xff89 = GDK_KEY_KP_Tab
  toEnum 0xff8d = GDK_KEY_KP_Enter
  toEnum 0xff91 = GDK_KEY_KP_F1
  toEnum 0xff92 = GDK_KEY_KP_F2
  toEnum 0xff93 = GDK_KEY_KP_F3
  toEnum 0xff94 = GDK_KEY_KP_F4
  toEnum 0xff95 = GDK_KEY_KP_Home
  toEnum 0xff96 = GDK_KEY_KP_Left
  toEnum 0xff97 = GDK_KEY_KP_Up
  toEnum 0xff98 = GDK_KEY_KP_Right
  toEnum 0xff99 = GDK_KEY_KP_Down
  toEnum 0xff9a = GDK_KEY_KP_Page_Up
  toEnum 0xff9b = GDK_KEY_KP_Page_Down
  toEnum 0xff9c = GDK_KEY_KP_End
  toEnum 0xff9d = GDK_KEY_KP_Begin
  toEnum 0xff9e = GDK_KEY_KP_Insert
  toEnum 0xff9f = GDK_KEY_KP_Delete
  toEnum 0xffaa = GDK_KEY_KP_Multiply
  toEnum 0xffab = GDK_KEY_KP_Add
  toEnum 0xffac = GDK_KEY_KP_Separator
  toEnum 0xffad = GDK_KEY_KP_Subtract
  toEnum 0xffae = GDK_KEY_KP_Decimal
  toEnum 0xffaf = GDK_KEY_KP_Divide
  toEnum 0xffb0 = GDK_KEY_KP_0
  toEnum 0xffb1 = GDK_KEY_KP_1
  toEnum 0xffb2 = GDK_KEY_KP_2
  toEnum 0xffb3 = GDK_KEY_KP_3
  toEnum 0xffb4 = GDK_KEY_KP_4
  toEnum 0xffb5 = GDK_KEY_KP_5
  toEnum 0xffb6 = GDK_KEY_KP_6
  toEnum 0xffb7 = GDK_KEY_KP_7
  toEnum 0xffb8 = GDK_KEY_KP_8
  toEnum 0xffb9 = GDK_KEY_KP_9
  toEnum 0xffbd = GDK_KEY_KP_Equal
  toEnum 0xffbe = GDK_KEY_F1
  toEnum 0xffbf = GDK_KEY_F2
  toEnum 0xffc0 = GDK_KEY_F3
  toEnum 0xffc1 = GDK_KEY_F4
  toEnum 0xffc2 = GDK_KEY_F5
  toEnum 0xffc3 = GDK_KEY_F6
  toEnum 0xffc4 = GDK_KEY_F7
  toEnum 0xffc5 = GDK_KEY_F8
  toEnum 0xffc6 = GDK_KEY_F9
  toEnum 0xffc7 = GDK_KEY_F10
  toEnum 0xffc8 = GDK_KEY_F11
  toEnum 0xffc9 = GDK_KEY_F12
  toEnum 0xffca = GDK_KEY_F13
  toEnum 0xffcb = GDK_KEY_F14
  toEnum 0xffcc = GDK_KEY_F15
  toEnum 0xffcd = GDK_KEY_F16
  toEnum 0xffce = GDK_KEY_F17
  toEnum 0xffcf = GDK_KEY_F18
  toEnum 0xffd0 = GDK_KEY_F19
  toEnum 0xffd1 = GDK_KEY_F20
  toEnum 0xffd2 = GDK_KEY_F21
  toEnum 0xffd3 = GDK_KEY_F22
  toEnum 0xffd4 = GDK_KEY_F23
  toEnum 0xffd5 = GDK_KEY_F24
  toEnum 0xffd6 = GDK_KEY_F25
  toEnum 0xffd7 = GDK_KEY_F26
  toEnum 0xffd8 = GDK_KEY_F27
  toEnum 0xffd9 = GDK_KEY_F28
  toEnum 0xffda = GDK_KEY_F29
  toEnum 0xffdb = GDK_KEY_F30
  toEnum 0xffdc = GDK_KEY_F31
  toEnum 0xffdd = GDK_KEY_F32
  toEnum 0xffde = GDK_KEY_F33
  toEnum 0xffdf = GDK_KEY_F34
  toEnum 0xffe0 = GDK_KEY_F35
  toEnum 0xffe1 = GDK_KEY_Shift_L
  toEnum 0xffe2 = GDK_KEY_Shift_R
  toEnum 0xffe3 = GDK_KEY_Control_L
  toEnum 0xffe4 = GDK_KEY_Control_R
  toEnum 0xffe5 = GDK_KEY_Caps_Lock
  toEnum 0xffe6 = GDK_KEY_Shift_Lock
  toEnum 0xffe7 = GDK_KEY_Meta_L
  toEnum 0xffe8 = GDK_KEY_Meta_R
  toEnum 0xffe9 = GDK_KEY_Alt_L
  toEnum 0xffea = GDK_KEY_Alt_R
  toEnum 0xffeb = GDK_KEY_Super_L
  toEnum 0xffec = GDK_KEY_Super_R
  toEnum 0xffed = GDK_KEY_Hyper_L
  toEnum 0xffee = GDK_KEY_Hyper_R
  toEnum 0xfff1 = GDK_KEY_braille_dot_1
  toEnum 0xfff2 = GDK_KEY_braille_dot_2
  toEnum 0xfff3 = GDK_KEY_braille_dot_3
  toEnum 0xfff4 = GDK_KEY_braille_dot_4
  toEnum 0xfff5 = GDK_KEY_braille_dot_5
  toEnum 0xfff6 = GDK_KEY_braille_dot_6
  toEnum 0xfff7 = GDK_KEY_braille_dot_7
  toEnum 0xfff8 = GDK_KEY_braille_dot_8
  toEnum 0xfff9 = GDK_KEY_braille_dot_9
  toEnum 0xfffa = GDK_KEY_braille_dot_10
  toEnum 0xffffff = GDK_KEY_VoidSymbol
  toEnum 0xffff = GDK_KEY_Delete
  toEnum _ = error "Invalid value for enum KeyCode"
  
