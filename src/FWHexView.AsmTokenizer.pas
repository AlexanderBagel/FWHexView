﻿////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.AsmTokenizer.pas
//  * Purpose   : Class for defining the type of token with which the string begins
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2025.
//  * Version   : 2.0.14
//  * Home Page : http://rouse.drkb.ru
//  * Home Blog : http://alexander-bagel.blogspot.ru
//  ****************************************************************************
//  * Latest Release : https://github.com/AlexanderBagel/FWHexView/releases
//  * Latest Source  : https://github.com/AlexanderBagel/FWHexView
//  ****************************************************************************
//

{
Licence:
  FWHexView is dual-licensed. You may choose to use it under the restrictions of the GPL v3 licence at no cost to you,
  or you may purchase a commercial licence. A commercial licence grants you the right to use FWHexView in your own
  applications, royalty free, and without any requirement to disclose your source code nor any modifications to FWHexView
  to any other party. A commercial licence lasts into perpetuity, and entitles you to all future updates, free of
  charge. A commercial licence is sold per developer developing applications that use FWHexView, as follows:
    1 developer = $49
    2 developers = $89
    3 developers = $139
    4 developers = $169
    5 developers = $199
    >5 developers = $199 + $25 per developer from the 6th onwards
    site licence = $499 (unlimited number of developers affiliated with the owner of the licence, i.e. employees, co-workers, interns and contractors)

  Please send an e-mail to hexview_sale@rousehome.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.
}

unit FWHexView.AsmTokenizer;

{$IFDEF FPC}
  {$I FWHexViewConfig.inc}
{$ENDIF}

interface

uses
  SysUtils,
  Generics.Collections;

type
  TTokenType = (ttUnknown, ttNumber, ttInstruction, ttReg, ttPrefix, ttJmp, ttKernel, ttNop, ttSize);
  TRegType = (rtUnknown, rtReg8, rtReg16, rtReg32, rtReg64, rtRegSeg, rtRegPtr, rtX87, rtSimd64, rtSimd128, rtSimd256, rtSimd512);

  { TAsmTokenizer }

  TAsmTokenizer = class
  private
    FTokens: TDictionary<string, TTokenType>;
    function IsNumber(const Value: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetToken(pData: PChar; var TokenLength: Integer): TTokenType;
    function GetRegType(const AReg: string): TRegType;
  end;

implementation

const InstBuff: array[0..1378] of string = (
  'AAA', 'AAD', 'AAM', 'AAS', 'ADC', 'ADCX', 'ADD', 'ADDPD', 'ADDPS', 'ADDSD',
  'ADDSS', 'ADDSUBPD', 'ADDSUBPS', 'ADOX', 'AESDEC', 'AESDEC128KL',
  'AESDEC256KL', 'AESDECLAST', 'AESDECWIDE128KL', 'AESDECWIDE256KL', 'AESENC',
  'AESENC128KL', 'AESENC256KL', 'AESENCLAST', 'AESENCWIDE128KL',
  'AESENCWIDE256KL', 'AESIMC', 'AESKEYGENASSIST', 'AND', 'ANDN', 'ANDNPD',
  'ANDNPS', 'ANDPD', 'ANDPS', 'BEXTR', 'BLENDPD', 'BLENDPS', 'BLENDVPD',
  'BLENDVPS', 'BLSI', 'BLSMSK', 'BLSR', 'BNDCL', 'BNDCN', 'BNDCU', 'BNDLDX',
  'BNDMK', 'BNDMOV', 'BNDSTX', 'BOUND', 'BSF', 'BSR', 'BSWAP', 'BT', 'BTC',
  'BTR', 'BTS', 'BZHI', 'CALLF', 'CBW', 'CCS_ENCRYPT', 'CCS_HASH', 'CDQ',
  'CDQE', 'CLC', 'CLD', 'CLDEMOTE', 'CLFLUSH', 'CLFLUSHOPT', 'CLI', 'CLUI',
  'CLWB', 'CLZERO', 'CMC', 'CMOVA', 'CMOVAE', 'CMOVB', 'CMOVBE', 'CMOVC',
  'CMOVCC', 'CMOVE', 'CMOVG', 'CMOVGE', 'CMOVL', 'CMOVLE', 'CMOVNA', 'CMOVNAE',
  'CMOVNB', 'CMOVNBE', 'CMOVNC', 'CMOVNE', 'CMOVNG', 'CMOVNGE', 'CMOVNL',
  'CMOVNLE', 'CMOVNO', 'CMOVNP', 'CMOVNS', 'CMOVNZ', 'CMOVO', 'CMOVP',
  'CMOVPE', 'CMOVPO', 'CMOVS', 'CMOVZ', 'CMP', 'CMPPD', 'CMPPS', 'CMPS',
  'CMPSB', 'CMPSD', 'CMPSQ', 'CMPSS', 'CMPSW', 'CMPXCHG', 'CMPXCHG16B',
  'CMPXCHG8B', 'COMISD', 'COMISS', 'CPUID', 'CQO', 'CRC32', 'CVTDQ2PD',
  'CVTDQ2PS', 'CVTPD2DQ', 'CVTPD2PI', 'CVTPD2PS', 'CVTPI2PD', 'CVTPI2PS',
  'CVTPS2DQ', 'CVTPS2PD', 'CVTPS2PI', 'CVTSD2SI', 'CVTSD2SS', 'CVTSI2SD',
  'CVTSI2SS', 'CVTSS2SD', 'CVTSS2SI', 'CVTTPD2DQ', 'CVTTPD2PI', 'CVTTPS2DQ',
  'CVTTPS2PI', 'CVTTSD2SI', 'CVTTSS2SI', 'CWD', 'CWDE', 'DAA', 'DAS', 'DEC',
  'DIV', 'DIVPD', 'DIVPS', 'DIVSD', 'DIVSS', 'DPPD', 'DPPS', 'EMMS',
  'ENCLU', 'ENCODEKEY128', 'ENCODEKEY256', 'ENDBR32', 'ENDBR64', 'ENQCMD',
  'ENTER', 'ESC', 'EXTRACTPS', 'EXTRQ', 'F2XM1', 'FABS', 'FADD', 'FADDP',
  'FBLD', 'FBSTP', 'FCHS', 'FCLEX', 'FCMOVB', 'FCMOVBE', 'FCMOVCC', 'FCMOVE',
  'FCMOVNB', 'FCMOVNBE', 'FCMOVNE', 'FCMOVNU', 'FCMOVU', 'FCOM', 'FCOMI',
  'FCOMIP', 'FCOMP', 'FCOMPP', 'FCOS', 'FDECSTP', 'FDISI', 'FDIV', 'FDIVP',
  'FDIVR', 'FDIVRP', 'FENI', 'FFREE', 'FFREEP', 'FIADD', 'FICOM', 'FICOMP',
  'FIDIV', 'FIDIVR', 'FILD', 'FIMUL', 'FINCSTP', 'FINIT', 'FIST', 'FISTP',
  'FISTTP', 'FISUB', 'FISUBR', 'FLD', 'FLD1', 'FLDCW', 'FLDENV', 'FLDL2E',
  'FLDL2T', 'FLDLG2', 'FLDLN2', 'FLDPI', 'FLDZ', 'FMUL', 'FMULP', 'FNCLEX',
  'FNDISI', 'FNENI', 'FNINIT', 'FNSAVE', 'FNSETPM', 'FNSTCW', 'FNSTENV',
  'FNSTSW', 'FPATAN', 'FPREM', 'FPREM1', 'FPTAN', 'FRNDINT', 'FRSTOR',
  'FSAVE', 'FSCALE', 'FSETPM', 'FSIN', 'FSINCOS', 'FSQRT', 'FST', 'FSTCW',
  'FSTENV', 'FSTP', 'FSTPNCE', 'FSTSW', 'FSUB', 'FSUBP', 'FSUBR', 'FSUBRP',
  'FTST', 'FUCOM', 'FUCOMI', 'FUCOMIP', 'FUCOMP', 'FUCOMPP', 'FWAIT', 'FXAM',
  'FXCH', 'FXRSTOR', 'FXSAVE', 'FXTRACT', 'FYL2X', 'FYL2XP1', 'HADDPD',
  'HADDPS', 'HINT_NOP', 'HLT', 'HSUBPD', 'HSUBPS', 'IBTS', 'ICEBP', 'IDIV',
  'IMUL', 'INC', 'INCSSPD', 'INCSSPQ', 'INSERTPS', 'INSERTQ', 'IRET', 'IRETD',
  'IRETQ', 'JCC', 'JMPE', 'JMPF', 'JRCXZ', 'KADDB', 'KADDD', 'KADDQ', 'KADDW',
  'KANDB', 'KANDD', 'KANDNB', 'KANDND', 'KANDNQ', 'KANDNW', 'KANDQ', 'KANDW',
  'KMOVB', 'KMOVD', 'KMOVQ', 'KMOVW', 'KNOTB', 'KNOTD', 'KNOTQ', 'KNOTW',
  'KORB', 'KORD', 'KORQ', 'KORTESTB', 'KORTESTD', 'KORTESTQ', 'KORTESTW',
  'KORW', 'KSHIFTLB', 'KSHIFTLD', 'KSHIFTLQ', 'KSHIFTLW', 'KSHIFTRB',
  'KSHIFTRD', 'KSHIFTRQ', 'KSHIFTRW', 'KTESTB', 'KTESTD', 'KTESTQ', 'KTESTW',
  'KUNPCKBW', 'KUNPCKDQ', 'KUNPCKWD', 'KXNORB', 'KXNORD', 'KXNORQ', 'KXNORW',
  'KXORB', 'KXORD', 'KXORQ', 'KXORW', 'LAHF', 'LDDQU', 'LDMXCSR', 'LDS', 'LEA',
  'LEAVE', 'LES', 'LFENCE', 'LFS', 'LGS', 'LOADIWKEY', 'LODS', 'LODSB',
  'LODSD', 'LODSQ', 'LODSW', 'LOOP', 'LOOPCC', 'LOOPE', 'LOOPNE', 'LOOPNZ',
  'LOOPZ', 'LSS', 'LZCNT', 'MASKMOVDQU', 'MASKMOVQ', 'MAXPD', 'MAXPS', 'MAXSD',
  'MAXSS', 'MCOMMIT', 'MFENCE', 'MINPD', 'MINPS', 'MINSD', 'MINSS', 'MONITORX',
  'MONTMUL', 'MOV', 'MOVABS', 'MOVAPD', 'MOVAPS', 'MOVBE', 'MOVD', 'MOVDDUP',
  'MOVDIR64B', 'MOVDIRI', 'MOVDQ2Q', 'MOVDQA', 'MOVDQU', 'MOVHLPS', 'MOVHPD',
  'MOVHPS', 'MOVLHPS', 'MOVLPD', 'MOVLPS', 'MOVMSKPD', 'MOVMSKPS', 'MOVNTDQ',
  'MOVNTDQA', 'MOVNTI', 'MOVNTPD', 'MOVNTPS', 'MOVNTQ', 'MOVNTSD', 'MOVNTSS',
  'MOVQ', 'MOVQ2DQ', 'MOVS', 'MOVSB', 'MOVSD', 'MOVSHDUP', 'MOVSLDUP', 'MOVSQ',
  'MOVSS', 'MOVSW', 'MOVSX', 'MOVSXD', 'MOVUPD', 'MOVUPS', 'MOVZX', 'MPSADBW',
  'MUL', 'MULPD', 'MULPS', 'MULSD', 'MULSS', 'MULX', 'MWAITX', 'NEG', 'NOT',
  'OIO', 'OR', 'ORPD', 'ORPS', 'PABSB', 'PABSD', 'PABSQ', 'PABSW', 'PACKSSDW',
  'PACKSSWB', 'PACKUSDW', 'PACKUSWB', 'PADDB', 'PADDD', 'PADDQ', 'PADDSB',
  'PADDSW', 'PADDUSB', 'PADDUSW', 'PADDW', 'PALIGNR', 'PAND', 'PANDN', 'PAUSE',
  'PAVGB', 'PAVGW', 'PBLENDVB', 'PBLENDW', 'PCLMULHQHQDQ', 'PCLMULHQLQDQ',
  'PCLMULLQHQDQ', 'PCLMULLQLQDQ', 'PCLMULQDQ', 'PCMPEQB', 'PCMPEQD', 'PCMPEQQ',
  'PCMPEQW', 'PCMPESTRI', 'PCMPESTRM', 'PCMPGTB', 'PCMPGTD', 'PCMPGTQ',
  'PCMPGTW', 'PCMPISTRI', 'PCMPISTRM', 'PDEP', 'PEXT', 'PEXTRB', 'PEXTRD',
  'PEXTRQ', 'PEXTRW', 'PHADDD', 'PHADDSW', 'PHADDW', 'PHMINPOSUW', 'PHSUBD',
  'PHSUBSW', 'PHSUBW', 'PINSRB', 'PINSRD', 'PINSRQ', 'PINSRW', 'PMADDUBSW',
  'PMADDWD', 'PMAXSB', 'PMAXSD', 'PMAXSQ', 'PMAXSW', 'PMAXUB', 'PMAXUD',
  'PMAXUQ', 'PMAXUW', 'PMINSB', 'PMINSD', 'PMINSQ', 'PMINSW', 'PMINUB',
  'PMINUD', 'PMINUQ', 'PMINUW', 'PMOVMSKB', 'PMOVSX', 'PMOVSXBD', 'PMOVSXBQ',
  'PMOVSXBW', 'PMOVSXDQ', 'PMOVSXWD', 'PMOVSXWQ', 'PMOVZX', 'PMOVZXBD',
  'PMOVZXBQ', 'PMOVZXBW', 'PMOVZXDQ', 'PMOVZXWD', 'PMOVZXWQ', 'PMULDQ',
  'PMULHRSW', 'PMULHUW', 'PMULHW', 'PMULLD', 'PMULLQ', 'PMULLW', 'PMULUDQ',
  'POP', 'POPA', 'POPAD', 'POPAL', 'POPCNT', 'POPF', 'POPFD', 'POPFQ', 'POR',
  'PREFETCH', 'PREFETCHH', 'PREFETCHNTA', 'PREFETCHT0', 'PREFETCHT1',
  'PREFETCHT2', 'PREFETCHW', 'PREFETCHWT1', 'PROLD', 'PROLQ', 'PROLVD',
  'PROLVQ', 'PRORD', 'PRORQ', 'PRORVD', 'PRORVQ', 'PSADBW', 'PSHUFB', 'PSHUFD',
  'PSHUFHW', 'PSHUFLW', 'PSHUFW', 'PSIGNB', 'PSIGND', 'PSIGNW', 'PSLLD',
  'PSLLDQ', 'PSLLQ', 'PSLLW', 'PSRAD', 'PSRAQ', 'PSRAW', 'PSRLD', 'PSRLDQ',
  'PSRLQ', 'PSRLW', 'PSUBB', 'PSUBD', 'PSUBQ', 'PSUBSB', 'PSUBSW', 'PSUBUSB',
  'PSUBUSW', 'PSUBW', 'PTEST', 'PTR', 'PTWRITE', 'PUNPCKHBW', 'PUNPCKHDQ',
  'PUNPCKHQDQ', 'PUNPCKHWD', 'PUNPCKLBW', 'PUNPCKLDQ', 'PUNPCKLQDQ',
  'PUNPCKLWD', 'PUSH', 'PUSHA', 'PUSHAD', 'PUSHAL', 'PUSHF', 'PUSHFD',
  'PUSHFQ', 'PXOR', 'RCL', 'RCPPS', 'RCPSS', 'RCR', 'RDFSBASE', 'RDGSBASE',
  'RDPID', 'RDPKRU', 'RDPMC', 'RDPRU', 'RDRAND', 'RDSEED', 'RDSSPD', 'RDSSPQ',
  'RDTSC', 'RDTSCP', 'RETF', 'RETN', 'REX', 'REX.B', 'REX.R', 'REX.RB',
  'REX.RX', 'REX.RXB', 'REX.W', 'REX.WB', 'REX.WR', 'REX.WRB', 'REX.WRX',
  'REX.WRXB', 'REX.WX', 'REX.WXB', 'REX.X', 'REX.XB', 'ROL', 'ROR', 'RORX',
  'ROUNDPD', 'ROUNDPS', 'ROUNDSD', 'ROUNDSS', 'RSQRTPS', 'RSQRTSS', 'RSTORSSP',
  'RTDSC', 'SAHF', 'SAL', 'SALC', 'SAR', 'SARX', 'SAVEPREVSSP', 'SBB', 'SCAS',
  'SCASB', 'SCASD', 'SCASQ', 'SCASW', 'SENDUIPI', 'SERIALIZE', 'SETA', 'SETAE',
  'SETALC', 'SETB', 'SETBE', 'SETC', 'SETCC', 'SETE', 'SETG', 'SETGE', 'SETL',
  'SETLE', 'SETNA', 'SETNAE', 'SETNB', 'SETNBE', 'SETNC', 'SETNE', 'SETNG',
  'SETNGE', 'SETNL', 'SETNLE', 'SETNO', 'SETNP', 'SETNS', 'SETNZ', 'SETO',
  'SETP', 'SETPE', 'SETPO', 'SETS', 'SETZ', 'SFENCE', 'SHA1MSG1', 'SHA1MSG2',
  'SHA1NEXTE', 'SHA1RNDS4', 'SHA256MSG1', 'SHA256MSG2', 'SHA256RNDS2', 'SHL',
  'SHLD', 'SHLX', 'SHR', 'SHRD', 'SHRX', 'SHUFPD', 'SHUFPS', 'SQRTPD',
  'SQRTPS', 'SQRTSD', 'SQRTSS', 'STC', 'STD', 'STI', 'STMXCSR', 'STOS',
  'STOSB', 'STOSD', 'STOSQ', 'STOSW', 'STUI', 'SUB', 'SUBPD', 'SUBPS', 'SUBSD',
  'SUBSS', 'TEST', 'TESTUI', 'TPAUSE', 'TZCNT', 'UCOMISD', 'UCOMISS', 'UD0',
  'UD1', 'UD2', 'UD2A', 'UD2B', 'UIRET', 'UMONITOR', 'UMOV', 'UMWAIT',
  'UNPCKHPD', 'UNPCKHPS', 'UNPCKLPD', 'UNPCKLPS', 'VADDPD', 'VADDPS', 'VADDSD',
  'VADDSS', 'VADDSUBPD', 'VADDSUBPS', 'VAESDEC', 'VAESDECLAST', 'VAESENC',
  'VAESENCLAST', 'VAESIMC', 'VAESKEYGENASSIST', 'VALIGND', 'VALIGNQ',
  'VANDNPD', 'VANDNPS', 'VANDPD', 'VANDPS', 'VBLENDMPD', 'VBLENDMPS',
  'VBLENDPD', 'VBLENDPS', 'VBLENDVPD', 'VBLENDVPS', 'VBROADCAST',
  'VBROADCASTF128', 'VBROADCASTF32X2', 'VBROADCASTF32X4', 'VBROADCASTF32X8',
  'VBROADCASTF64X2', 'VBROADCASTF64X4', 'VBROADCASTI128', 'VBROADCASTI32X2',
  'VBROADCASTI32X8', 'VBROADCASTI64X4', 'VBROADCASTSD', 'VBROADCASTSS',
  'VCMPPD', 'VCMPPS', 'VCMPSD', 'VCMPSS', 'VCOMISD', 'VCOMISS', 'VCOMPRESSPD',
  'VCOMPRESSPS', 'VCVTDQ2PD', 'VCVTDQ2PS', 'VCVTPD2DQ', 'VCVTPD2PS',
  'VCVTPD2QQ', 'VCVTPD2UDQ', 'VCVTPD2UQQ', 'VCVTPH2PS', 'VCVTPS2DQ',
  'VCVTPS2PD', 'VCVTPS2PH', 'VCVTPS2QQ', 'VCVTPS2UDQ', 'VCVTPS2UQQ',
  'VCVTQQ2PD', 'VCVTQQ2PS', 'VCVTSD2SI', 'VCVTSD2SS', 'VCVTSD2USI',
  'VCVTSI2SD', 'VCVTSI2SS', 'VCVTSS2SD', 'VCVTSS2SI', 'VCVTSS2USI',
  'VCVTTPD2DQ', 'VCVTTPD2QQ', 'VCVTTPD2UDQ', 'VCVTTPD2UQQ', 'VCVTTPS2DQ',
  'VCVTTPS2QQ', 'VCVTTPS2UDQ', 'VCVTTPS2UQQ', 'VCVTTSD2SI', 'VCVTTSD2USI',
  'VCVTTSS2SI', 'VCVTTSS2USI', 'VCVTUDQ2PD', 'VCVTUDQ2PS', 'VCVTUQQ2PD',
  'VCVTUQQ2PS', 'VCVTUSI2SD', 'VCVTUSI2SS', 'VDBPSADBW', 'VDIVPD', 'VDIVPS',
  'VDIVSD', 'VDIVSS', 'VDPPD', 'VDPPS', 'VEXP2PD', 'VEXP2PS', 'VEXPANDPD',
  'VEXPANDPS', 'VEXTRACTF128', 'VEXTRACTF32X4', 'VEXTRACTF32X8',
  'VEXTRACTF64X2', 'VEXTRACTF64X4', 'VEXTRACTI128', 'VEXTRACTI32X4',
  'VEXTRACTI32X8', 'VEXTRACTI64X2', 'VEXTRACTI64X4', 'VEXTRACTPS',
  'VFIXUPIMMPD', 'VFIXUPIMMPS', 'VFIXUPIMMSD', 'VFIXUPIMMSS', 'VFMADD132PD',
  'VFMADD132PS', 'VFMADD132SD', 'VFMADD132SS', 'VFMADD213PD', 'VFMADD213PS',
  'VFMADD213SD', 'VFMADD213SS', 'VFMADD231PD', 'VFMADD231PS', 'VFMADD231SD',
  'VFMADD231SS', 'VFMADDSUB132PD', 'VFMADDSUB132PS', 'VFMADDSUB213PD',
  'VFMADDSUB213PS', 'VFMADDSUB231PD', 'VFMADDSUB231PS', 'VFMSUB132PD',
  'VFMSUB132PS', 'VFMSUB132SD', 'VFMSUB132SS', 'VFMSUB213PD', 'VFMSUB213PS',
  'VFMSUB213SD', 'VFMSUB213SS', 'VFMSUB231PD', 'VFMSUB231PS', 'VFMSUB231SD',
  'VFMSUB231SS', 'VFMSUBADD132PD', 'VFMSUBADD132PS', 'VFMSUBADD213PD',
  'VFMSUBADD213PS', 'VFMSUBADD231PD', 'VFMSUBADD231PS', 'VFNMADD132PD',
  'VFNMADD132PS', 'VFNMADD132SD', 'VFNMADD132SS', 'VFNMADD213PD',
  'VFNMADD213PS', 'VFNMADD213SD', 'VFNMADD213SS', 'VFNMADD231PD',
  'VFNMADD231PS', 'VFNMADD231SD', 'VFNMADD231SS', 'VFNMSUB132PD',
  'VFNMSUB132PS', 'VFNMSUB132SD', 'VFNMSUB132SS', 'VFNMSUB213PD',
  'VFNMSUB213PS', 'VFNMSUB213SD', 'VFNMSUB213SS', 'VFNMSUB231PD',
  'VFNMSUB231PS', 'VFNMSUB231SD', 'VFNMSUB231SS', 'VFPCLASSPD', 'VFPCLASSPS',
  'VFPCLASSSD', 'VFPCLASSSS', 'VGATHERDPD', 'VGATHERDPS', 'VGATHERPF0DPD',
  'VGATHERPF0DPS', 'VGATHERPF0QPD', 'VGATHERPF0QPS', 'VGATHERPF1DPD',
  'VGATHERPF1DPS', 'VGATHERPF1QPD', 'VGATHERPF1QPS', 'VGATHERQPD',
  'VGATHERQPS', 'VGETEXPPD', 'VGETEXPPS', 'VGETEXPSD', 'VGETEXPSS',
  'VGETMANTPD', 'VGETMANTPS', 'VGETMANTSD', 'VGETMANTSS', 'VHADDPD', 'VHADDPS',
  'VHSUBPD', 'VHSUBPS', 'VINSERTF128', 'VINSERTF32X4', 'VINSERTF32X8',
  'VINSERTF64X2', 'VINSERTF64X4', 'VINSERTI128', 'VINSERTI32X4',
  'VINSERTI32X8', 'VINSERTI64X2', 'VINSERTI64X4', 'VINSERTPS', 'VLDDQU',
  'VLDMXCSR', 'VMASKMOV', 'VMASKMOVDQU', 'VMASKMOVPD', 'VMASKMOVPS', 'VMAXPD',
  'VMAXPS', 'VMAXSD', 'VMAXSS', 'VMINPD', 'VMINPS', 'VMINSD', 'VMINSS',
  'VMOVAPD', 'VMOVAPS', 'VMOVD', 'VMOVDDUP', 'VMOVDQA', 'VMOVDQA32',
  'VMOVDQA64', 'VMOVDQU', 'VMOVDQU16', 'VMOVDQU32', 'VMOVDQU64', 'VMOVDQU8',
  'VMOVHLPS', 'VMOVHPD', 'VMOVHPS', 'VMOVLHPS', 'VMOVLPD', 'VMOVLPS',
  'VMOVMSKPD', 'VMOVMSKPS', 'VMOVNTDQ', 'VMOVNTDQA', 'VMOVNTPD', 'VMOVNTPS',
  'VMOVQ', 'VMOVSD', 'VMOVSHDUP', 'VMOVSLDUP', 'VMOVSS', 'VMOVUPD', 'VMOVUPS',
  'VMPSADBW', 'VMULPD', 'VMULPS', 'VMULSD', 'VMULSS', 'VORPD', 'VORPS',
  'VPABSB', 'VPABSD', 'VPABSQ', 'VPABSW', 'VPACKSSDW', 'VPACKSSWB',
  'VPACKUSDW', 'VPACKUSWB', 'VPADDB', 'VPADDD', 'VPADDQ', 'VPADDSB', 'VPADDSW',
  'VPADDUSB', 'VPADDUSW', 'VPADDW', 'VPALIGNR', 'VPAND', 'VPANDD', 'VPANDN',
  'VPANDND', 'VPANDNQ', 'VPANDQ', 'VPAVGB', 'VPAVGW', 'VPBLENDD', 'VPBLENDMB',
  'VPBLENDMD', 'VPBLENDMQ', 'VPBLENDMW', 'VPBLENDVB', 'VPBLENDW',
  'VPBROADCAST', 'VPBROADCASTB', 'VPBROADCASTD', 'VPBROADCASTM',
  'VPBROADCASTMB2Q', 'VPBROADCASTMW2D', 'VPBROADCASTQ', 'VPBROADCASTW',
  'VPCLMULQDQ', 'VPCMPB', 'VPCMPD', 'VPCMPEQB', 'VPCMPEQD', 'VPCMPEQQ',
  'VPCMPEQW', 'VPCMPESTRI', 'VPCMPESTRM', 'VPCMPGTB', 'VPCMPGTD', 'VPCMPGTQ',
  'VPCMPGTW', 'VPCMPISTRI', 'VPCMPISTRM', 'VPCMPQ', 'VPCMPUB', 'VPCMPUD',
  'VPCMPUQ', 'VPCMPUW', 'VPCMPW', 'VPCOMPRESSD', 'VPCOMPRESSQ', 'VPCONFLICTD',
  'VPCONFLICTQ', 'VPERM2F128', 'VPERM2I128', 'VPERMD', 'VPERMI2D', 'VPERMI2PD',
  'VPERMI2PS', 'VPERMI2Q', 'VPERMI2W', 'VPERMILPD', 'VPERMILPS', 'VPERMPD',
  'VPERMPS', 'VPERMQ', 'VPERMW', 'VPEXPANDD', 'VPEXPANDQ', 'VPEXTRB',
  'VPEXTRD', 'VPEXTRQ', 'VPEXTRW', 'VPGATHERDD', 'VPGATHERDQ', 'VPGATHERQD',
  'VPGATHERQQ', 'VPHADDD', 'VPHADDSW', 'VPHADDW', 'VPHMINPOSUW', 'VPHSUBD',
  'VPHSUBSW', 'VPHSUBW', 'VPINSRB', 'VPINSRD', 'VPINSRQ', 'VPINSRW',
  'VPLZCNTD', 'VPLZCNTQ', 'VPMADDUBSW', 'VPMADDWD', 'VPMASKMOV', 'VPMASKMOVD',
  'VPMASKMOVQ', 'VPMAXSB', 'VPMAXSD', 'VPMAXSQ', 'VPMAXSW', 'VPMAXUB',
  'VPMAXUD', 'VPMAXUQ', 'VPMAXUW', 'VPMINSB', 'VPMINSD', 'VPMINSQ', 'VPMINSW',
  'VPMINUB', 'VPMINUD', 'VPMINUQ', 'VPMINUW', 'VPMOVB2M', 'VPMOVD2M',
  'VPMOVDB', 'VPMOVDW', 'VPMOVM2B', 'VPMOVM2D', 'VPMOVM2Q', 'VPMOVM2W',
  'VPMOVMSKB', 'VPMOVQ2M', 'VPMOVQB', 'VPMOVQD', 'VPMOVQW', 'VPMOVSDB',
  'VPMOVSDW', 'VPMOVSQB', 'VPMOVSQD', 'VPMOVSQW', 'VPMOVSWB', 'VPMOVSXBD',
  'VPMOVSXBQ', 'VPMOVSXBW', 'VPMOVSXDQ', 'VPMOVSXWD', 'VPMOVSXWQ', 'VPMOVUSDB',
  'VPMOVUSDW', 'VPMOVUSQB', 'VPMOVUSQD', 'VPMOVUSQW', 'VPMOVUSWB', 'VPMOVW2M',
  'VPMOVWB', 'VPMOVZXBD', 'VPMOVZXBQ', 'VPMOVZXBW', 'VPMOVZXDQ', 'VPMOVZXWD',
  'VPMOVZXWQ', 'VPMULDQ', 'VPMULHRSW', 'VPMULHUW', 'VPMULHW', 'VPMULLD',
  'VPMULLQ', 'VPMULLW', 'VPMULUDQ', 'VPOR', 'VPORD', 'VPORQ', 'VPROLD',
  'VPROLQ', 'VPROLVD', 'VPROLVQ', 'VPRORD', 'VPRORQ', 'VPRORVD', 'VPRORVQ',
  'VPSADBW', 'VPSCATTERDD', 'VPSCATTERDQ', 'VPSCATTERQD', 'VPSCATTERQQ',
  'VPSHUFB', 'VPSHUFD', 'VPSHUFHW', 'VPSHUFLW', 'VPSIGNB', 'VPSIGND',
  'VPSIGNW', 'VPSLLD', 'VPSLLDQ', 'VPSLLQ', 'VPSLLVD', 'VPSLLVQ', 'VPSLLVW',
  'VPSLLW', 'VPSRAD', 'VPSRAQ', 'VPSRAVD', 'VPSRAVQ', 'VPSRAVW', 'VPSRAW',
  'VPSRLD', 'VPSRLDQ', 'VPSRLQ', 'VPSRLVD', 'VPSRLVQ', 'VPSRLVW', 'VPSRLW',
  'VPSUBB', 'VPSUBD', 'VPSUBQ', 'VPSUBSB', 'VPSUBSW', 'VPSUBUSB', 'VPSUBUSW',
  'VPSUBW', 'VPTERNLOGD', 'VPTERNLOGQ', 'VPTEST', 'VPTESTMB', 'VPTESTMD',
  'VPTESTMQ', 'VPTESTMW', 'VPTESTNMB', 'VPTESTNMD', 'VPTESTNMQ', 'VPTESTNMW',
  'VPUNPCKHBW', 'VPUNPCKHDQ', 'VPUNPCKHQDQ', 'VPUNPCKHWD', 'VPUNPCKLBW',
  'VPUNPCKLDQ', 'VPUNPCKLQDQ', 'VPUNPCKLWD', 'VPXOR', 'VPXORD', 'VPXORQ',
  'VRANGEPD', 'VRANGEPS', 'VRANGESD', 'VRANGESS', 'VRCP14PD', 'VRCP14PS',
  'VRCP14SD', 'VRCP14SS', 'VRCP28PD', 'VRCP28PS', 'VRCP28SD', 'VRCP28SS',
  'VRCPPS', 'VRCPSS', 'VREDUCEPD', 'VREDUCEPS', 'VREDUCESD', 'VREDUCESS',
  'VRNDSCALEPD', 'VRNDSCALEPS', 'VRNDSCALESD', 'VRNDSCALESS', 'VROUNDPD',
  'VROUNDPS', 'VROUNDSD', 'VROUNDSS', 'VRSQRT14PD', 'VRSQRT14PS', 'VRSQRT14SD',
  'VRSQRT14SS', 'VRSQRT28PD', 'VRSQRT28PS', 'VRSQRT28SD', 'VRSQRT28SS',
  'VRSQRTPS', 'VRSQRTSS', 'VSCALEFPD', 'VSCALEFPS', 'VSCALEFSD', 'VSCALEFSS',
  'VSCATTERDPD', 'VSCATTERDPS', 'VSCATTERPF0DPD', 'VSCATTERPF0DPS',
  'VSCATTERPF0QPD', 'VSCATTERPF0QPS', 'VSCATTERPF1DPD', 'VSCATTERPF1DPS',
  'VSCATTERPF1QPD', 'VSCATTERPF1QPS', 'VSCATTERQPD', 'VSCATTERQPS',
  'VSHUFF32X4', 'VSHUFF64X2', 'VSHUFI32X4', 'VSHUFI64X2', 'VSHUFPD', 'VSHUFPS',
  'VSQRTPD', 'VSQRTPS', 'VSQRTSD', 'VSQRTSS', 'VSTMXCSR', 'VSUBPD', 'VSUBPS',
  'VSUBSD', 'VSUBSS', 'VTESTPD', 'VTESTPS', 'VUCOMISD', 'VUCOMISS',
  'VUNPCKHPD', 'VUNPCKHPS', 'VUNPCKLPD', 'VUNPCKLPS', 'VXORPD', 'VXORPS',
  'VZEROALL', 'VZEROUPPER', 'WAIT', 'WRFSBASE', 'WRGSBASE', 'WRPKRU', 'WRSSD',
  'WRSSQ', 'XABORT', 'XACQUIRE', 'XADD', 'XBEGIN', 'XBTS', 'XCHG', 'XCRYPTCBC',
  'XCRYPTCFB', 'XCRYPTCTR', 'XCRYPTECB', 'XCRYPTOFB', 'XEND', 'XGETBV', 'XLAT',
  'XLATB', 'XOR', 'XORPD', 'XORPS', 'XRELEASE', 'XRESLDTRK', 'XRSTOR',
  'XRSTOR64', 'XSAVE', 'XSAVE64', 'XSAVEC', 'XSAVEC64', 'XSAVEOPT',
  'XSAVEOPT64', 'XSHA1', 'XSHA256', 'XSTORE', 'XSUSLDTRK', 'XTEST'
  );

const SizeBuff: array[0..7] of string = (
  'BYTE', 'WORD', 'DWORD', 'QWORD', 'TBYTE', 'XMMWORD', 'YMMWORD', 'ZMMWORD');

const JmpBuff: array[0..34] of string = (
  'CALL', 'JA', 'JAE', 'JB', 'JBE', 'JC', 'JCXZ', 'JE', 'JECXZ', 'JG', 'JGE', 'JL',
  'JLE', 'JMP', 'JNA', 'JNAE', 'JNB', 'JNBE', 'JNC', 'JNE', 'JNG', 'JNGE', 'JNL', 'JNLE',
  'JNO', 'JNP', 'JNS', 'JNZ', 'JO', 'JP', 'JPE', 'JPO', 'JS', 'JZ', 'RET'
  );

const KernelBuff: array[0..100] of string = (
  'ARPL', 'CLAC', 'CLGI', 'CLRSSBSY', 'CLTS', 'DB', 'ENCLS', 'ENCLV', 'ENQCMDS', 'GETSEC',
  'HRESET', 'IN', 'INS', 'INSB', 'INSW', 'INSD', 'INT', 'INT01', 'INT1', 'INT3', 'INTO',
  'INVD', 'INVEPT', 'INVLPG', 'INVLPGA', 'INVLPGB', 'INVPCID', 'INVVPID',
  'LAR', 'LGDT', 'LIDT', 'LLDT', 'LMSW', 'LOADALL', 'LOADALL386', 'LOADALLD', 'LSL',
  'LTR', 'MONITOR', 'MWAIT', 'OUT', 'OUTS', 'OUTSB', 'OUTSW', 'OUTSD', 'PCONFIG', 'PSMASH',
  'PVALIDATE', 'RDMSR', 'RMPADJUST', 'RMPQUERY', 'RMPUPDATE', 'RSM', 'SEAMCALL', 'SEAMOPS',
  'SEAMRET', 'SETSSBSY', 'SGDT', 'SIDT', 'SKINIT', 'SLDT', 'SMSW', 'STAC', 'STGI', 'STOREALL',
  'STR', 'SWAPGS', 'SYSCALL', 'SYSENTER', 'SYSEXIT', 'SYSRET', 'TDCALL', 'TLBSYNC', 'VERR',
  'VERW', 'VMCALL', 'VMCLEAR', 'VMFUNC', 'VMGEXIT', 'VMLAUNCH', 'VMLOAD', 'VMMCALL', 'VMPTRLD',
  'VMPTRST', 'VMREAD', 'VMRESUME', 'VMRUN', 'VMSAVE', 'VMWRITE', 'VMXOFF', 'VMXON', 'WBINVD',
  'WBNOINVD', 'WRMSR', 'WRUSSD', 'WRUSSQ', 'XRSTORS', 'XRSTORS64', 'XSAVES', 'XSAVES64', 'XSETBV'
  );

const PrefixBuff: array[0..6] of string = (
  'LOCK', 'NOTRACK', 'REP', 'REPE', 'REPNE', 'REPNZ', 'REPZ'
  );

const RegsBuff: array[0..137] of string = (
  'AH', 'AL', 'AX', 'BH', 'BL', 'BP', 'BPL', 'BX', 'CH', 'CL', 'CR0', 'CR2', 'CR3',
  'CR4', 'CR8', 'CS', 'CX', 'DH', 'DI', 'DIL', 'DL', 'DR0', 'DR1', 'DR2', 'DR3', 'DR6',
  'DR7', 'DS', 'DX', 'EAX', 'EBP', 'EBX', 'ECX', 'EDI', 'EDX', 'EIP', 'ES', 'ESI', 'ESP',
  'FS', 'GS', 'MM0', 'MM1', 'MM2', 'MM3', 'MM4', 'MM5', 'MM6', 'MM7', 'R10', 'R10B',
  'R10D', 'R10W', 'R11', 'R11B', 'R11D', 'R11W', 'R12', 'R12B', 'R12D', 'R12W', 'R13',
  'R13B', 'R13D', 'R13W', 'R14', 'R14B', 'R14D', 'R14W', 'R15', 'R15B', 'R15D', 'R15W',
  'R8', 'R8B', 'R8D', 'R8W', 'R9', 'R9B', 'R9D', 'R9W', 'RAX', 'RBP', 'RBX', 'RCX',
  'RDI', 'RDX', 'RIP', 'RSI', 'RSP', 'SI', 'SIL', 'SP', 'SPL', 'SS', 'ST(0)', 'ST(1)',
  'ST(2)', 'ST(3)', 'ST(4)', 'ST(5)', 'ST(6)', 'ST(7)', 'XMM0', 'XMM1', 'XMM10', 'XMM11', 'XMM12',
  'XMM13', 'XMM14', 'XMM15', 'XMM2', 'XMM3', 'XMM4', 'XMM5', 'XMM6', 'XMM7', 'XMM8',
  'XMM9', 'YMM0', 'YMM1', 'YMM10', 'YMM11', 'YMM12', 'YMM13', 'YMM14', 'YMM15', 'YMM2',
  'YMM3', 'YMM4', 'YMM5', 'YMM6', 'YMM7', 'YMM8', 'YMM9',
  'RFLAGS', 'EFLAGS', 'FLAGS'
  );

const RegTypesBuff: array[0..137] of TRegType = (
  // 'AH',   'AL',   'AX',    'BH',   'BL',   'BP',    'BPL',  'BX',   'CH',    'CL',
     rtReg8, rtReg8, rtReg16, rtReg8, rtReg8, rtReg16, rtReg8, rtReg16, rtReg8, rtReg8,
  // 'CR0',  'CR2',    'CR3',   'CR4',   'CR8',
     rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  // 'CS',     'CX',    'DH',   'DI',    'DIL',  'DL',
     rtRegSeg, rtReg16, rtReg8, rtReg16, rtReg8, rtReg8,
  // 'DR0',    'DR1',    'DR2',    'DR3',    'DR6',    'DR7',
     rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr, rtRegPtr,
  // 'DS',     'DX',    'EAX',   'EBP',   'EBX',   'ECX',   'EDI',   'EDX',   'EIP',
     rtRegSeg, rtReg16, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32, rtReg32,
  //  'ES',    'ESI',   'ESP',   'FS',     'GS',     'MM0',    'MM1',    'MM2',
     rtRegSeg, rtReg32, rtReg32, rtRegSeg, rtRegSeg, rtSimd64, rtSimd64, rtSimd64,
  // 'MM3',    'MM4',    'MM5',    'MM6',    'MM7',    'R10',   'R10B', 'R10D',
     rtSimd64, rtSimd64, rtSimd64, rtSimd64, rtSimd64, rtReg64, rtReg8, rtReg32,
  // 'R10W',  'R11',   'R11B', 'R11D',  'R11W',  'R12',   'R12B', 'R12D',  'R12W',
     rtReg16, rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg8, rtReg32, rtReg16,
  // 'R13',   'R13B', 'R13D',  'R13W',  'R14',   'R14B', 'R14D',  'R14W',
     rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg8, rtReg32, rtReg16,
  // 'R15',   'R15B', 'R15D',  'R15W',  'R8',    'R8B',  'R8D',   'R8W',
     rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg8, rtReg32, rtReg16,
  // 'R9',    'R9B',  'R9D',   'R9W',   'RAX',   'RBP',   'RBX',   'RCX',
     rtReg64, rtReg8, rtReg32, rtReg16, rtReg64, rtReg64, rtReg64, rtReg64,
  // 'RDI',   'RDX',   'RIP',   'RSI',   'RSP',   'SI',    'SIL',  'SP',    'SPL',
     rtReg64, rtReg64, rtReg64, rtReg64, rtReg64, rtReg16, rtReg8, rtReg16, rtReg8,
  // 'SS',     'ST0', 'ST1', 'ST2', 'ST3', 'ST4', 'ST5', 'ST6', 'ST7',
     rtRegSeg, rtX87, rtX87, rtX87, rtX87, rtX87, rtX87, rtX87, rtX87,
  // 'XMM0',    'XMM1',    'XMM10',   'XMM11',   'XMM12',   'XMM13',   'XMM14',
     rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  // 'XMM15',   'XMM2',    'XMM3',    'XMM4',    'XMM5',    'XMM6',    'XMM7',
     rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128, rtSimd128,
  // 'XMM8',    'XMM9',    'YMM0',    'YMM1',    'YMM10',   'YMM11',   'YMM12',
     rtSimd128, rtSimd128, rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256,
  // 'YMM13',   'YMM14',   'YMM15',   'YMM2',    'YMM3',    'YMM4',    'YMM5',
     rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256, rtSimd256,
  // 'YMM6',    'YMM7',    'YMM8',    'YMM9'
     rtSimd256, rtSimd256, rtSimd256, rtSimd256,
  // 'RFLAGS','EFLAGS','FLAGS'
     rtReg64, rtReg32, rtReg16
  );

const NopBuff: array[0..3] of string = (
  'FNOP', 'NOP', 'NOPL', 'NOPW'
  );

{ TAsmTokenizer }

constructor TAsmTokenizer.Create;
var
  I: Integer;
begin
  FTokens := TDictionary<string, TTokenType>.Create;
  for I := 0 to Length(InstBuff) - 1 do
    FTokens.Add(InstBuff[I], ttInstruction);
  for I := 0 to Length(SizeBuff) - 1 do
    FTokens.Add(SizeBuff[I], ttSize);
  for I := 0 to Length(RegsBuff) - 1 do
    FTokens.Add(RegsBuff[I], ttReg);
  for I := 0 to Length(PrefixBuff) - 1 do
    FTokens.Add(PrefixBuff[I], ttPrefix);
  for I := 0 to Length(JmpBuff) - 1 do
    FTokens.Add(JmpBuff[I], ttJmp);
  for I := 0 to Length(KernelBuff) - 1 do
    FTokens.Add(KernelBuff[I], ttKernel);
  for I := 0 to Length(NopBuff) - 1 do
    FTokens.Add(NopBuff[I], ttNop);

end;

destructor TAsmTokenizer.Destroy;
begin
  FTokens.Free;
  inherited;
end;

function TAsmTokenizer.GetToken(pData: PChar;
  var TokenLength: Integer): TTokenType;

  function IsTokenChar(Value: PChar): Boolean;
  begin
    Result := False;
    case Value^ of
      'A'..'Z', 'a'..'z', '0'..'9', '(', ')', '$': Result := True;
    end;
  end;

var
  pCursor: PChar;
  FoundToken: string;
  CharLeft: Integer;
  FirstIsTokenChar: Boolean;
begin
  Result := ttUnknown;
  pCursor := pData;
  CharLeft := 0;
  FirstIsTokenChar := IsTokenChar(pCursor);
  while (FirstIsTokenChar = IsTokenChar(pCursor)) and (CharLeft < TokenLength) do
  begin
    Inc(pCursor);
    Inc(CharLeft);
  end;
  TokenLength := CharLeft;
  if not FirstIsTokenChar then
    Exit;
  FoundToken := Copy(pData, 1, CharLeft);
  if IsNumber(FoundToken) then
    Result := ttNumber
  else
    FTokens.TryGetValue(FoundToken, Result);
  if pCursor^ = ' ' then
    Inc(TokenLength);
end;

function TAsmTokenizer.GetRegType(const AReg: string): TRegType;
var
  I: Integer;
begin
  Result := rtUnknown;
  for I := 0 to Length(RegsBuff) - 1 do
    if AReg = RegsBuff[I] then
      Exit(RegTypesBuff[I]);
end;

function TAsmTokenizer.IsNumber(const Value: string): Boolean;
var
  Tmp: Int64;
begin
  Result := TryStrToInt64(Value, Tmp);
end;

end.
