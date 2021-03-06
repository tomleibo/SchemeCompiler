WRITE_SOB_SYMBOL:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);
  PUSH(R3);
  MOV(R0, INDD(FPARG(0), 1));
  MOV(R1, INDD(R0, 1));
  MOV(R2, R0);
  ADD(R2, IMM(2));
 L_WSS_LOOP_S:
  CMP(R1, IMM(0));
  JUMP_EQ(L_WSS_EXIT_S);
  CMP(IND(R2), '\n');
  JUMP_EQ(L_WSS_NEWLINE_S);
  CMP(IND(R2), '\t');
  JUMP_EQ(L_WSS_TAB_S);
  CMP(IND(R2), '\f');
  JUMP_EQ(L_WSS_PAGE_S);
  CMP(IND(R2), '\r');
  JUMP_EQ(L_WSS_RETURN_S);
  CMP(IND(R2), '\\');
  JUMP_EQ(L_WSS_BACKSLASH_S);
  CMP(IND(R2), '\"');
  JUMP_EQ(L_WSS_DQUOTE_S);
  CMP(IND(R2), ' ');
  JUMP_LT(L_WSS_OCT_CHAR_S);
  PUSH(IND(R2));
  CALL(PUTCHAR);
  DROP(1);
  JUMP(L_WSS_LOOP_CONT_S);
 L_WSS_DQUOTE_S:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\"'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_S);
 L_WSS_BACKSLASH_S:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_S);
 L_WSS_RETURN_S:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('r'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_S);
 L_WSS_PAGE_S:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('f'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_S);
 L_WSS_TAB_S:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('t'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_S);  
 L_WSS_NEWLINE_S:
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  PUSH(IMM('n'));
  CALL(PUTCHAR);
  DROP(2);
  JUMP(L_WSS_LOOP_CONT_S);
 L_WSS_OCT_CHAR_S:
  MOV(R0, IND(R2));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  MOV(R3, R0);
  REM(R3, IMM(8));
  PUSH(R3);
  DIV(R0, IMM(8));
  REM(R0, IMM(8));
  PUSH(R0);
  PUSH(IMM('\\'));
  CALL(PUTCHAR);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
  CALL(WRITE_INTEGER);
  DROP(1);
 L_WSS_LOOP_CONT_S:
  INCR(R2);
  DECR(R1);
  JUMP(L_WSS_LOOP_S);
 L_WSS_EXIT_S:
  POP(R3);
  POP(R2);
  POP(R1);
  POP(FP);
  RETURN;

