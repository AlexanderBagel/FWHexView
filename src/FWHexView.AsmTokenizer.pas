////////////////////////////////////////////////////////////////////////////////
//
//  ****************************************************************************
//  * Project   : Hex Viewer Project
//  * Unit Name : FWHexView.AsmTokenizer.pas
//  * Purpose   : Class for defining the type of token with which the string begins
//  * Author    : Alexander (Rouse_) Bagel
//  * Copyright : © Fangorn Wizards Lab 1998 - 2024.
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

  Please send an e-mail to rouse79@yandex.ru to request an invoice before or after payment is made. Payment may be
  made via bank transfer. Bank details will be provided on the invoice.

  Support (via e-mail) is available for users with a commercial licence. Enhancement requests submitted by users with a
  commercial licence will be prioritized.
}

unit FWHexView.AsmTokenizer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils,
  Generics.Collections;

type
  TTokenType = (ttUnknown, ttNumber, ttInstruction, ttReg, ttPrefix, ttJmp, ttKernel, ttNop);

  TAsmTokenizer = class
  private
    FTokens: TDictionary<string, TTokenType>;
    function IsNumber(const Value: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetToken(pData: PChar; var TokenLength: Integer): TTokenType;
  end;

implementation

const InstBuff: array[0..727] of string = (
  'AAA', 'AAD', 'AAM', 'AAS', 'ADC', 'ADCX', 'ADD', 'ADDPD', 'ADDPS', 'ADDSD', 'ADDSS',
  'ADDSUBPD', 'ADDSUBPS', 'ADOX', 'AESDEC', 'AESDEC128KL', 'AESDEC256KL', 'AESDECLAST',
  'AESDECWIDE128KL', 'AESDECWIDE256KL', 'AESENC', 'AESENC128KL', 'AESENC256KL', 'AESENCLAST',
  'AESENCWIDE128KL', 'AESENCWIDE256KL', 'AESIMC', 'AESKEYGENASSIST', 'AND', 'ANDN',
  'ANDNPD', 'ANDNPS', 'ANDPD', 'ANDPS', 'BEXTR', 'BLENDPD', 'BLENDPS', 'BLENDVPD',
  'BLENDVPS', 'BLSI', 'BLSMSK', 'BLSR', 'BOUND', 'BSF', 'BSR', 'BSWAP', 'BT', 'BTC',
  'BTR', 'BTS', 'BZHI', 'CBW', 'CCS_ENCRYPT', 'CCS_HASH', 'CDQ', 'CDQE', 'CLC', 'CLD',
  'CLDEMOTE', 'CLFLUSH', 'CLFLUSHOPT', 'CLI', 'CLUI', 'CLWB', 'CLZERO', 'CMC', 'CMOVA',
  'CMOVAE', 'CMOVB', 'CMOVBE', 'CMOVC', 'CMOVE', 'CMOVG', 'CMOVGE', 'CMOVL', 'CMOVLE',
  'CMOVNA', 'CMOVNAE', 'CMOVNB', 'CMOVNBE', 'CMOVNC', 'CMOVNE', 'CMOVNG', 'CMOVNGE',
  'CMOVNL', 'CMOVNLE', 'CMOVNO', 'CMOVNP', 'CMOVNS', 'CMOVNZ', 'CMOVO', 'CMOVP', 'CMOVPE',
  'CMOVPO', 'CMOVS', 'CMOVZ', 'CMP', 'CMPPD', 'CMPPS', 'CMPSB', 'CMPSD', 'CMPSQ', 'CMPSS',
  'CMPSW', 'CMPXCHG', 'CMPXCHG16B', 'CMPXCHG8B', 'COMISD', 'COMISS', 'CPUID', 'CQO',
  'CRC32', 'CVTDQ2PD', 'CVTDQ2PS', 'CVTPD2DQ', 'CVTPD2PI', 'CVTPD2PS', 'CVTPI2PD',
  'CVTPI2PS', 'CVTPS2DQ', 'CVTPS2PD', 'CVTPS2PI', 'CVTSD2SI', 'CVTSD2SS', 'CVTSI2SD',
  'CVTSI2SS', 'CVTSS2SD', 'CVTSS2SI', 'CVTTPD2DQ', 'CVTTPD2PI', 'CVTTPS2DQ', 'CVTTPS2PI',
  'CVTTSD2SI', 'CVTTSS2SI', 'CWD', 'CWDE', 'DAA', 'DAS', 'DEC', 'DIV', 'DIVPD', 'DIVPS',
  'DIVSD', 'DIVSS', 'DPPD', 'DPPS', 'EMMS', 'ENCLU', 'ENCODEKEY128', 'ENCODEKEY256',
  'ENDBR32', 'ENDBR64', 'ENQCMD', 'ENTER', 'ESC', 'EXTRACTPS', 'EXTRQ', 'F2XM1', 'FABS',
  'FADD', 'FADDP', 'FBLD', 'FBSTP', 'FCHS', 'FCLEX', 'FCMOVB', 'FCMOVBE', 'FCMOVE',
  'FCMOVNB', 'FCMOVNBE', 'FCMOVNE', 'FCMOVNU', 'FCMOVU', 'FCOM', 'FCOMI', 'FCOMIP',
  'FCOMP', 'FCOMPP', 'FCOS', 'FDECSTP', 'FDISI', 'FDIV', 'FDIVP', 'FDIVR', 'FDIVRP',
  'FENI', 'FFREE', 'FFREEP', 'FIADD', 'FICOM', 'FICOMP', 'FIDIV', 'FIDIVR', 'FILD',
  'FIMUL', 'FINCSTP', 'FINIT', 'FIST', 'FISTP', 'FISTTP', 'FISUB', 'FISUBR', 'FLD',
  'FLD1', 'FLDCW', 'FLDENV', 'FLDL2E', 'FLDL2T', 'FLDLG2', 'FLDLN2', 'FLDPI', 'FLDZ',
  'FMUL', 'FMULP', 'FNCLEX', 'FNDISI', 'FNENI', 'FNINIT', 'FNSAVE', 'FNSETPM',
  'FNSTCW', 'FNSTENV', 'FNSTSW', 'FPATAN', 'FPREM', 'FPREM1', 'FPTAN', 'FRNDINT', 'FRSTOR',
  'FSAVE', 'FSCALE', 'FSETPM', 'FSIN', 'FSINCOS', 'FSQRT', 'FST', 'FSTCW', 'FSTENV',
  'FSTP', 'FSTPNCE', 'FSTSW', 'FSUB', 'FSUBP', 'FSUBR', 'FSUBRP', 'FTST', 'FUCOM',
  'FUCOMI', 'FUCOMIP', 'FUCOMP', 'FUCOMPP', 'FWAIT', 'FXAM', 'FXCH', 'FXRSTOR', 'FXSAVE',
  'FXTRACT', 'FYL2X', 'FYL2XP1', 'HADDPD', 'HADDPS', 'HLT', 'HSUBPD', 'HSUBPS', 'IBTS',
  'ICEBP', 'IDIV', 'IMUL', 'INC', 'INCSSPD', 'INCSSPQ', 'INSERTPS', 'INSERTQ',
  'IRET', 'IRETD', 'IRETQ', 'JRCXZ', 'LAHF',
  'LDDQU', 'LDMXCSR', 'LDS', 'LEA', 'LEAVE', 'LES', 'LFENCE', 'LFS', 'LGS', 'LOADIWKEY',
  'LODSB', 'LODSD', 'LODSQ', 'LODSW', 'LOOP', 'LOOPE', 'LOOPNE', 'LOOPNZ',
  'LOOPZ', 'LSS', 'LZCNT', 'MASKMOVDQU', 'MASKMOVQ', 'MAXPD', 'MAXPS', 'MAXSD', 'MAXSS',
  'MCOMMIT', 'MFENCE', 'MINPD', 'MINPS', 'MINSD', 'MINSS', 'MONITORX', 'MONTMUL', 'MOV',
  'MOVAPD', 'MOVAPS', 'MOVBE', 'MOVD', 'MOVDDUP', 'MOVDIR64B', 'MOVDIRI', 'MOVDQ2Q',
  'MOVDQA', 'MOVDQU', 'MOVHLPS', 'MOVHPD', 'MOVHPS', 'MOVLHPS', 'MOVLPD', 'MOVLPS',
  'MOVMSKPD', 'MOVMSKPS', 'MOVNTDQ', 'MOVNTDQA', 'MOVNTI', 'MOVNTPD', 'MOVNTPS', 'MOVNTQ',
  'MOVNTSD', 'MOVNTSS', 'MOVQ', 'MOVQ2DQ', 'MOVSB', 'MOVSD', 'MOVSHDUP', 'MOVSLDUP',
  'MOVSQ', 'MOVSS', 'MOVSW', 'MOVSXD', 'MOVUPD', 'MOVUPS', 'MOVZX', 'MPSADBW', 'MUL',
  'MULPD', 'MULPS', 'MULSD', 'MULSS', 'MULX', 'MWAITX', 'NEG', 'NOT',
  'OIO', 'OR', 'ORPD', 'ORPS', 'PABSB', 'PABSD', 'PABSW',
  'PACKSSDW', 'PACKSSWB', 'PACKUSDW', 'PACKUSWB', 'PADDB', 'PADDD', 'PADDQ', 'PADDSB',
  'PADDSW', 'PADDUSB', 'PADDUSW', 'PADDW', 'PALIGNR', 'PAND', 'PANDN', 'PAUSE', 'PAVGB',
  'PAVGW', 'PBLENDVB', 'PBLENDW', 'PCLMULHQHQDQ', 'PCLMULHQLQDQ', 'PCLMULLQHQDQ', 'PCLMULLQLQDQ',
  'PCLMULQDQ', 'PCMPEQB', 'PCMPEQD', 'PCMPEQQ', 'PCMPEQW', 'PCMPESTRI', 'PCMPESTRM',
  'PCMPGTB', 'PCMPGTD', 'PCMPGTQ', 'PCMPGTW', 'PCMPISTRI', 'PCMPISTRM', 'PDEP', 'PEXT',
  'PEXTRB', 'PEXTRD', 'PEXTRQ', 'PEXTRW', 'PHADDD', 'PHADDSW', 'PHADDW', 'PHMINPOSUW',
  'PHSUBD', 'PHSUBSW', 'PHSUBW', 'PINSRB', 'PINSRD', 'PINSRQ', 'PINSRW', 'PMADDUBSW',
  'PMADDWD', 'PMAXSB', 'PMAXSD', 'PMAXSW', 'PMAXUB', 'PMAXUD', 'PMAXUW', 'PMINSB',
  'PMINSD', 'PMINSW', 'PMINUB', 'PMINUD', 'PMINUW', 'PMOVMSKB', 'PMOVSXBD', 'PMOVSXBQ',
  'PMOVSXBW', 'PMOVSXDQ', 'PMOVSXWD', 'PMOVSXWQ', 'PMOVZXBD', 'PMOVZXBQ', 'PMOVZXBW',
  'PMOVZXDQ', 'PMOVZXWD', 'PMOVZXWQ', 'PMULDQ', 'PMULHRSW', 'PMULHUW', 'PMULHW', 'PMULLD',
  'PMULLW', 'PMULUDQ', 'POP', 'POPA', 'POPAD', 'POPCNT', 'POPF', 'POPFD', 'POPFQ',
  'POR', 'PREFETCH', 'PREFETCHNTA', 'PREFETCHT0', 'PREFETCHT1', 'PREFETCHT2', 'PREFETCHW',
  'PREFETCHWT1', 'PSADBW', 'PSHUFB', 'PSHUFD', 'PSHUFHW', 'PSHUFLW', 'PSHUFW', 'PSIGNB',
  'PSIGND', 'PSIGNW', 'PSLLD', 'PSLLDQ', 'PSLLQ', 'PSLLW', 'PSRAD', 'PSRAW', 'PSRLD',
  'PSRLDQ', 'PSRLQ', 'PSRLW', 'PSUBB', 'PSUBD', 'PSUBQ', 'PSUBSB', 'PSUBSW', 'PSUBUSB',
  'PSUBUSW', 'PSUBW', 'PTEST', 'PTWRITE', 'PUNPCKHBW', 'PUNPCKHDQ', 'PUNPCKHQDQ', 'PUNPCKHWD',
  'PUNPCKLBW', 'PUNPCKLDQ', 'PUNPCKLQDQ', 'PUNPCKLWD', 'PUSH', 'PUSHA', 'PUSHAD', 'PUSHF',
  'PUSHFD', 'PUSHFQ', 'PXOR', 'RCL', 'RCPPS', 'RCPSS', 'RCR', 'RDFSBASE', 'RDGSBASE',
  'RDPID', 'RDPKRU', 'RDPMC', 'RDPRU', 'RDRAND', 'RDSEED', 'RDSSPD', 'RDSSPQ', 'RDTSC',
  'RDTSCP', 'RETF', 'RETN', 'ROL', 'ROR', 'RORX', 'ROUNDPD', 'ROUNDPS', 'ROUNDSD',
  'ROUNDSS', 'RSQRTPS', 'RSQRTSS', 'RSTORSSP', 'SAHF', 'SAL', 'SALC', 'SAR', 'SARX',
  'SAVEPREVSSP', 'SBB', 'SCASB', 'SCASD', 'SCASQ', 'SCASW', 'SENDUIPI', 'SERIALIZE',
  'SETA', 'SETAE', 'SETALC', 'SETB', 'SETBE', 'SETC', 'SETE', 'SETG', 'SETGE', 'SETL',
  'SETLE', 'SETNA', 'SETNAE', 'SETNB', 'SETNBE', 'SETNC', 'SETNE', 'SETNG', 'SETNGE',
  'SETNL', 'SETNLE', 'SETNO', 'SETNP', 'SETNS', 'SETNZ', 'SETO', 'SETP', 'SETPE', 'SETPO',
  'SETS', 'SETZ', 'SFENCE', 'SHA1MSG1', 'SHA1MSG2', 'SHA1NEXTE', 'SHA1RNDS4', 'SHA256MSG1',
  'SHA256MSG2', 'SHA256RNDS2', 'SHL', 'SHLD', 'SHLX', 'SHR', 'SHRD', 'SHRX', 'SHUFPD',
  'SHUFPS', 'SQRTPD', 'SQRTPS', 'SQRTSD', 'SQRTSS', 'STC', 'STD', 'STI', 'STMXCSR',
  'STOSB', 'STOSD', 'STOSQ', 'STOSW', 'STUI', 'SUB', 'SUBPD', 'SUBPS', 'SUBSD', 'SUBSS',
  'TEST', 'TESTUI', 'TPAUSE', 'TZCNT', 'UCOMISD', 'UCOMISS', 'UD0', 'UD1', 'UD2', 'UD2A',
  'UD2B', 'UIRET', 'UMONITOR', 'UMOV', 'UMWAIT', 'UNPCKHPD', 'UNPCKHPS', 'UNPCKLPD',
  'UNPCKLPS', 'VBROADCASTF128', 'VBROADCASTI128', 'VBROADCASTSD', 'VBROADCASTSS', 'VCVTPH2PS',
  'VCVTPS2PH', 'VEXTRACTF128', 'VEXTRACTI128', 'VGATHERDPD', 'VGATHERDPS', 'VGATHERQPD',
  'VGATHERQPS', 'VINSERTF128', 'VINSERTI128', 'VMASKMOVPD', 'VMASKMOVPS', 'VPBLENDD',
  'VPBROADCASTB', 'VPBROADCASTD', 'VPBROADCASTQ', 'VPBROADCASTW', 'VPERM2F128', 'VPERM2I128',
  'VPERMD', 'VPERMILPD', 'VPERMILPS', 'VPERMPD', 'VPERMPS', 'VPERMQ', 'VPGATHERDD',
  'VPGATHERDQ', 'VPGATHERQD', 'VPGATHERQQ', 'VPMASKMOVD', 'VPMASKMOVQ', 'VPSLLVD',
  'VPSLLVQ', 'VPSRAVD', 'VPSRLVD', 'VPSRLVQ', 'VZEROALL', 'VZEROUPPER', 'WAIT', 'WRFSBASE',
  'WRGSBASE', 'WRPKRU', 'WRSSD', 'WRSSQ', 'XABORT', 'XACQUIRE', 'XADD', 'XBEGIN', 'XBTS',
  'XCHG', 'XCRYPTCBC', 'XCRYPTCFB', 'XCRYPTCTR', 'XCRYPTECB', 'XCRYPTOFB', 'XEND',
  'XGETBV', 'XLAT', 'XOR', 'XORPD', 'XORPS', 'XRELEASE', 'XRESLDTRK', 'XRSTOR', 'XRSTOR64',
  'XSAVE', 'XSAVE64', 'XSAVEC', 'XSAVEC64', 'XSAVEOPT', 'XSAVEOPT64', 'XSHA1', 'XSHA256',
  'XSTORE', 'XSUSLDTRK', 'XTEST',
  'BYTE', 'WORD', 'DWORD', 'QWORD', 'TBYTE', 'PTR'
  );

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

const RegsBuff: array[0..133] of string = (
  'AH', 'AL', 'AX', 'BH', 'BL', 'BP', 'BPL', 'BX', 'CH', 'CL', 'CR0', 'CR2', 'CR3',
  'CR4', 'CR8', 'CS', 'CX', 'DH', 'DI', 'DIL', 'DL', 'DR0', 'DR1', 'DR2', 'DR3', 'DR6',
  'DR7', 'DS', 'DX', 'EAX', 'EBP', 'EBX', 'ECX', 'EDI', 'EDX', 'ES', 'ESI', 'ESP',
  'FS', 'GS', 'MM0', 'MM1', 'MM2', 'MM3', 'MM4', 'MM5', 'MM6', 'MM7', 'R10', 'R10B',
  'R10D', 'R10W', 'R11', 'R11B', 'R11D', 'R11W', 'R12', 'R12B', 'R12D', 'R12W', 'R13',
  'R13B', 'R13D', 'R13W', 'R14', 'R14B', 'R14D', 'R14W', 'R15', 'R15B', 'R15D', 'R15W',
  'R8', 'R8B', 'R8D', 'R8W', 'R9', 'R9B', 'R9D', 'R9W', 'RAX', 'RBP', 'RBX', 'RCX',
  'RDI', 'RDX', 'RIP', 'RSI', 'RSP', 'SI', 'SIL', 'SP', 'SPL', 'SS', 'ST0', 'ST1',
  'ST2', 'ST3', 'ST4', 'ST5', 'ST6', 'ST7', 'XMM0', 'XMM1', 'XMM10', 'XMM11', 'XMM12',
  'XMM13', 'XMM14', 'XMM15', 'XMM2', 'XMM3', 'XMM4', 'XMM5', 'XMM6', 'XMM7', 'XMM8',
  'XMM9', 'YMM0', 'YMM1', 'YMM10', 'YMM11', 'YMM12', 'YMM13', 'YMM14', 'YMM15', 'YMM2',
  'YMM3', 'YMM4', 'YMM5', 'YMM6', 'YMM7', 'YMM8', 'YMM9'
  );

const NopBuff: array[0..2] of string = (
  'FNOP', 'NOP', 'NOPL'
  );

{ TAsmTokenizer }

constructor TAsmTokenizer.Create;
var
  I: Integer;
begin
  FTokens := TDictionary<string, TTokenType>.Create;
  for I := 0 to Length(InstBuff) - 1 do
    FTokens.Add(InstBuff[I], ttInstruction);
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
      'A'..'Z', 'a'..'z', '0'..'9': Result := True;
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

function TAsmTokenizer.IsNumber(const Value: string): Boolean;
var
  Tmp: Int64;
begin
  Result := TryStrToInt64(Value, Tmp);
end;

end.
