.extern printf
.extern scanf
.extern fgets
.global main	
main:
	.align 4
	ldr r0, =lr
	str lr, [r0]								@salvando lr, eh o que pc deve ser para sair do programa
	
		
	ldr r0, =printf_comeco
	bl printf
    @@@@@@@@@@@@@@@@@@INICIALIZANDO MEMORIA COM 0 @@@@@@@@@@@@@@@
    ldr r1, =inicio_mem
    mov r2, #0
    mov r3, #0									@contador
    
inicializar_mem:  
    str r2, [r1]								@OP1
    add r1, r1, #4
    
    str r2, [r1]								@ARG OP1
    add r1, r1, #4
    
    str r2, [r1]								@OP2
    add r1, r1, #4
    
    str r2, [r1]								@ARG OP2
    add r1, r1, #4
    
    add r3, r3, #1
    cmp r3, #1024
    blt inicializar_mem
    @@@@@@@@@@@@@@@@@@ COPIANDO MEMORIA DO IAS @@@@@@@@@@@@@@@@@@
    
	mov r3, #4									@r3 faz pular de 4 em 4 o espaco de memoria a ser escrito
	@ldr r1, =inicio_mem						@coloca o endereco a ser escrito como inicio_mem
	@sub r1, #4									@subtraindo para na primeira iteracao escrever em 0
	mov r5, #0									@conta numero de linhas da memoria

copiando_memoria:
	
	@/// OP1=endereco*16 /// ARG1=endereco*16+4 /// OP2=endereco*16+8 /// ARG2=endereco*16+12
    @@@@@ Salvando o endereco @@@@@
    ldr r0, =scanf_mask							@carrega a mascara de scanf em r0
    push {r1,r3}
	ldr r1, =endereco							
	bl scanf									@escreve em endereco
	pop {r1,r3}

	@@@@@ Verificando se o endereco eh valido @@@@
	push {r0, r1}
	ldr r0, =endereco
	ldr r0, [r0]
	bl test_addr
	pop {r0, r1}


	@@@@@ Salvando o OP 1 em inicio_mem[endereco] @@@@@
	ldr r0, =endereco							@carregando ponteiro para endereco
	ldr r0, [r0]								@carregando valor do endereco
	mov r6, #16
	mul r4, r0, r6								@r4 = endereco * 16
	ldr r1, =inicio_mem							@coloca o endereco a ser escrito como inicio_mem
	add r1, r1, r4								@r1 = r1(=inicio_mem) + endereco*16 (r4)
	ldr r0, =scanf_mask							@carrega a mascara de scanf em r0
	push {r1,r3}								
	bl scanf									@escreve em inicio_mem + 4
	pop {r1,r3}
	
	@@@@@ Salvando o argumento de OP 1 @@@@@
	ldr r0, =scanf_mask							@carrega a mascara de scanf em r0
	add r1, r1, r3								@aumento 4 para escrever no proximo espaco de memoria
	push {r1,r3}
	bl scanf									@escreve em inicio_mem + 8 bytes
	pop {r1,r3}

	@@@@@ Salvando o OP 2 @@@@@
	ldr r0, =scanf_mask							@carrega a mascara de scanf em r0
	add r1, r1, r3								@aumento 4 para escrever no proximo espaco de memoria
	push {r1,r3}
	bl scanf									@escreve em inicio_mem + 12 bytes
	pop {r1,r3}

	@@@@@ Salvando o argumento de OP 2 @@@@@
	ldr r0, =scanf_mask							@carrega a mascara de scanf em r0
	add r1, r1, r3								@aumento 4 para escrever no proximo espaco de memoria
	push {r1,r3}
	bl scanf									@escreve em inicio_mem + 16 bytes
	pop {r1,r3}
	
	
	cmp r0, #0
	add r5, #1
	bgt copiando_memoria
	sub r5, #1									@por algum motivo ele coloca 
	ldr r4, =numero_linhas						@o EOF em um outra linha
	str r5, [r4]

    @@@@@@@@@@@@@@@@@@ FIM DE COPIANDO MEMORIA DO IAS @@@@@@@@@@@@@@@@@@

	@@@@@Carregando e inicilizando os registradores ac=mq=jump=first=error=0@@@@@
	mov r0, #0
	ldr r1, =ac
	str r0, [r1]
	ldr r1, =mq
	str r0, [r1]
	ldr r1, =pc
	str r0, [r1]
	ldr r1, =jump
	str r0, [r1]
	ldr r1, =first
	str r0, [r1]
	ldr r1, =error
	str r0, [r1]
	@@@@@Fim da inicializacao@@@@@
	
	@@@@@IMPRIMINDO @ Estado inicial @@@@@
	@+ AC:  0x00000000     MQ: 0x00000000        PC: 0x00000000
	ldr r0, =printf_init
	bl printf
	ldr r0, =printf_state
	@r1 = ac & 0xFFFFFFFFFF, r2 = mq & 0xFFFFFFFFFF, r3 = pc & 0xFFFFFFFFFF
	ldr r1, =ac
	ldr r1, [r1]
	and r1, r1, #0xFFFFFFFF
	ldr r2, =mq
	ldr r2, [r2]
	and r2, r2, #0xFFFFFFFF
	ldr r3, =pc
	ldr r3, [r3]
	and r3, r3, #0xFFFFFFFF
	bl printf
	@@@@@FIM DA IMPRESSÃO@@@@@
	


	@@@@@@COMECO for (j = first; j < 2 && jump == 0 && error == 0; j++)@@@@@
	inicio_for:
	@ j = first
	ldr r0, =j
	ldr r1, =first
	ldr r1, [r1]
	str r1, [r0]
	comeco_for:
	@se j < 2 && jump == 0 && error == 0
	ldr r0, =j
	ldr r0, [r0]
	cmp r0, #2					@j<2
	ldrge r0, =j					@atualizando j
	ldrge r1, [r0]
	addge r1, r1, #1
	strge r1, [r0]	
	bge fim_switch
	ldr r0, =jump
	ldr r0, [r0]				@jump == 0
	cmp r0, #0
	ldrne r0, =j					@atualizando j
	ldrne r1, [r0]
	addne r1, r1, #1
	strne r1, [r0]	
	bne fim_switch
	ldr r0, =error
	ldr r0, [r0]
	cmp r0, #0					@error == 0
	ldrne r0, =j					@atualizando j
	ldrne r1, [r0]
	addne r1, r1, #1
	strne r1, [r0]	
	bne fim_switch

	
	
	@@@@ESCREVENDO printf("@ Executando instrucao no endereco %010X ",	(unsigned int)pc);@@@@
	ldr r0, =printf_pc
	ldr r1, =pc
	ldr r1, [r1]
	bl printf
	
	
	@COMPARANDO if (j == 0) 
	ldr r0, =j
	ldr r0, [r0]
	cmp r0, #0
	@SE NAO IR para instru_dir
	bne instru_dir  			@ELSE
		@SE SIM:
		@inst = (memory[pc] >> 20) & 0xFFFFF;
		ldr r0, =pc					@r0 =&pc
		ldr r0, [r0]				@r0=pc
		mov r1, #16
		mul r5, r0, r1				@r0=pc*16
		mov r0, r5					@r0=pc*16
		ldr r1, =inicio_mem			@r0 = pc*16 + &inicio_mem
		add r0, r0, r1				@r0 = pc*16 + &inicio_mem
		@add r0, r0, #4				@r0 = pc*16 + &inicio_mem + 4 = &op1
		mov r1, r0
		add r1, r1, #4				@r1 = pc*16 + &inicio_mem + 8 = &(arg op1)
		ldr r0, [r0]				@r0 = *(pc*16 + &inicio_mem +4) = op1
		ldr r1, [r1]				@r1 = *(pc*16 + &inicio_mem + 8) = arg op1
		@inst deve conter op1 concatenado com arg op1, vamos realizar isso
		@primeiro vamos colocar op1, shiftalo 3*4=12 bits para esquerda e 
		@depois fazer um and bitwise com o arg op1
		mov r3, r0					@r3 = op1
		mov r3, r3, lsl #12			@r3 = OP ???
		orr r3, r3, r1				@r3 = OP ARG = OP1 e ARG OP1 concatenados 
		ldr r0, =inst
		str r3, [r0]
	
		ldr r0, =printf_inst_esq
		bl printf
		@SAINDO DO IF
		b cont						@Voltando para o codigo principal
	
			@ENTRANDO NO ELSE
			@Falhando , fazer inst = memory[pc] & 0xFFFFF;
			@printf("(instrucao a direita)\n");
			instru_dir:
	
			ldr r0, =pc					@r0 =&pc
			ldr r0, [r0]				@r0=pc
			mov r1, #16
			mul r5, r0, r1				@r0=pc*16
			mov r0, r5					@r0=pc*16
			ldr r1, =inicio_mem			@r0 = pc*16 + &inicio_mem
			add r0, r0, r1				@r0 = pc*16 + &inicio_mem
			add r0, r0, #8				@r0 = pc*16 + &inicio_mem + 8 = &op2
			mov r1, r0
			add r1, r1, #4				@r1 = pc*16 + &inicio_mem + 12 = &(arg op2)
			ldr r0, [r0]				@r0 = *(pc*16 + &inicio_mem +8) = op2
			ldr r1, [r1]				@r1 = *(pc*16 + &inicio_mem + 12) = arg op2
			@inst deve conter op1 concatenado com arg op1, vamos realizar isso
			@primeiro vamos colocar op1, shiftalo 3*4=12 bits para esquerda e 
			@depois fazer um and bitwise com o arg op1
			mov r3, r0					@r3 = op1
			mov r3, r3, lsl #12			@r3 = OP ???
			orr r3, r3, r1				@r3 = OP ARG = OP1 e ARG OP1 concatenados 
			ldr r0, =inst
			str r3, [r0]
	
			ldr r0, =printf_inst_dir
			bl printf
			@SAINDO DO ELSE
			b cont						@Voltando para o codigo principal

						@@@@@@@@@@@@@@@SWITCH@@@@@@@@@@@@@@
		@@@A Pegando o endereco de memoria de onde operar@@@
		cont: @addr = inst & 0x0FFF; //getting only 3 last digits = addr to operate on
		ldr r0, =inst				@carregando o valor de inst
		ldr r0, [r0]				
		ldr r1, =fff				@carregando valor 0xFFF em r1
		ldr r1, [r1]
		and r0, r0, r1				@pegando primeiros 3 hexas de inst
		ldr r1, =addr
		str r0, [r1]				@salvando em addr
	
			@FAZENDO O SWITCH BASEADO NO OP switch (inst >> 12) { //has operation code	
	
			ldr r0, =inst				@inst = OP code concatenado com endereco
			ldr r0, [r0]				@carregando inst em r0
			mov r0, r0, lsr #12			@colocando r0 = OP
	
			@@@@SE OP_LOAD @@@
			@OP_LOAD = 0x01				
			cmp r0, #0x01
			beq op_load
			@@@@OP_LOAD = 0x01
	
			@@@@@SE OP_LOADMQM@@@
			@OP_LOADMQM = 0x09
			cmp r0, #0x09
			beq op_loadmqm
			@@@@@OP_LOADMQM

			@@@@@SE OP_LOADMQ@@@
			@OP_LOADMQ = 0x0A
			cmp r0, #0x0A
			beq op_loadmq
			@@@@@OP_LOADMQ@@@
			
			@@@@SE OP_LOADABS@@@
			@OP_LOADABS = 0x03
			cmp r0, #0x03
			beq op_loadabs
			@@@@@OP_LOADABS@@@@
			
			
			@@@@SE OP_LOADN@@@						
			@OP_LOADN = 0x02
			cmp r0, #0x02
			beq op_loadn
			@@@@@OP_LOADN@@@@
			
			
			@@@@SE OP_STOR@@@						
			OP_LOADABS = 0x21
			cmp r0, #0x21
			beq op_stor
			@@@@@OP_STOR@@@@
			
			@@@@SE OP_STORL@@@						
			OP_STORL  = 0x12
			cmp r0, #0x12
			beq op_storl
			@@@@@OP_STOR@@@@
			
			@@@@SE OP_STORR@@@	 					
			OP_STORR = 0x13
			cmp r0, #0x13
			beq op_storr
			@@@@@OP_STORR@@@@
			

	
			@@@@SE OP_ADD@@@	 					
			OP_ADD = 0x05
			cmp r0, #0x05
			beq op_add
			@@@@@OP_ADD@@@@
			
			@@@@SE OP_ADDABS@@@	 					
			OP_ADDABS = 0x07
			cmp r0, #0x07
			beq op_addabs
			@@@@OP_ADDABS@@@@
			
			@@@@SE OP_SUB@@@	 					
			OP_SUB = 0x06
			cmp r0, #0x06
			beq op_sub
			@@@@@OP_SUB@@@@
			
			@@@@SE OP_SUBABS@@@	 					
			OP_SUBABS = 0x08
			cmp r0, #0x08
			beq op_subabs
			@@@@@OP_STORR@@@@
			
			@@@@SE OP_MUL@@@	 					
			OP_MUL = 0x0B
			cmp r0, #0x0B
			beq op_mul
			@@@@@OP_MUL@@@@
			
			@@@@SE OP_DIV@@@	 					
			OP_STORR = 0x0C
			cmp r0, #0x0C
			beq op_div
			@@@@@OP_DIV@@@@
			
			@@@@SE OP_RSH@@@	 					
			OP_RSH = 0x15
			cmp r0, #0x15
			beq op_rsh
			@@@@@OP_RSH@@@@
						
			@@@@SE OP_LSH@@@	 					
			OP_RSH = 0x14
			cmp r0, #0x14
			beq op_lsh
			@@@@@OP_LSH@@@@
			
			@@@@SE OP_JUMPL@@@	 					
			OP_JUMPL = 0x0D
			cmp r0, #0x0D
			beq op_jumpl
			@@@@@OP_JUMPL@@@@
			
			@@@@SE OP_JUMPR@@@	 					
			OP_JUMPR = 0x0E
			cmp r0, #0x0E
			beq op_jumpr
			@@@@@OP_JUMPR@@@@
			
			@@@@SE OP_JUMPPL@@@	 					
			OP_JUMPPL = 0x0F
			cmp r0, #0x0F
			beq op_jumppl
			@@@@@OP_JUMPPL@@@@
			
			@@@@SE OP_JUMPPR@@@	 					
			OP_JUMPPR = 0x10
			cmp r0, #0x10
			beq op_jumppr
			@@@@@OP_JUMPPR@@@@
			
				
			@@@@@SE NADA @@@@
			@bl sair
			ldr r0, =printf_error_inst_errada
			ldr r1, =inst
			ldr r1, [r1]
			mov r1, r1, lsr #12
			bl printf
			b sair
			@@@@NADA@@@@@@@@
	
			@@@@@@@OP_LOADMQM@@@@@@@	
			op_loadmqm:
			ldr r0, =printf_op_loadmqm
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			@TESTANDO ENDERECO
			pop {r1}					@r1 tem o endereco
			mov r0, r1					@r0 = endereco
			push {r1}					@preservando r1
			bl test_addr
			pop {r1}
			@@FAZENDO MQ = MEMORY[ADDR]
			mov r5, #16					
			mul r0, r1, r5				@r0 = endereco(r1)*16
			ldr r1, =inicio_mem
			add r1, r1, r0				@r1 = inicio_mem + endereco*16 = &OP1
			add r1, #4					@r1 = inicio_mem + endereco*16 + 4 = &ARG OP 1
			ldr r2, [r1]				@r2 = ARG OP 1
			mov r2, r2, lsl #20			@r2 = (ARG OP 1) 00 000
			add r1, #4					@r1 = &(inicio_mem + endereco*16 + 4) = &OP 2
			ldr r3, [r1]				@r3 = (inicio_mem + endereco*16 + 4) = OP 2
			mov r3, r3, lsl #12			@r3 = 000 (OP 2) 000
			add r1, #4
			ldr r4, [r1]
			orr r2, r2, r3
			orr r2, r2, r4				@r2 = valor a ter ac
			ldr r1, =mq
			str r2, [r1]
			b fim_for
			@@@@@@@OP_LOADMQM@@@@@@@	

			@@@@@@@OP_LOAD@@@@@@@	
			@PRINTF@
			op_load:
			ldr r0, =printf_op_load
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			@TESTANDO ENDERECO
			pop {r1}					@r1 tem o endereco
			mov r0, r1					@r0 = endereco
			push {r1}					@preservando r1
			bl test_addr
			pop {r1}
			@@FAZENDO AC = MEMORY[ADDR] 
			mov r5, #16					
			mul r0, r1, r5				@r0 = endereco(r1)*16
			ldr r1, =inicio_mem
			add r1, r1, r0				@r1 = inicio_mem + endereco*16 = &OP1
			add r1, #4					@r1 = inicio_mem + endereco*16 + 4 = &ARG OP 1
			ldr r2, [r1]				@r2 = ARG OP 1
			mov r2, r2, lsl #20			@r2 = (ARG OP 1) 00 000
			add r1, #4					@r1 = &(inicio_mem + endereco*16 + 4) = &OP 2
			ldr r3, [r1]				@r3 = (inicio_mem + endereco*16 + 4) = OP 2
			mov r3, r3, lsl #12			@r3 = 000 (OP 2) 000
			add r1, #4
			ldr r4, [r1]
			orr r2, r2, r3
			orr r2, r2, r4				@r2 = valor a ter ac
			ldr r1, =ac
			str r2, [r1]
			mov r0, #0
			b fim_for
			@@@@@@@OP_LOAD@@@@@@@	


			@@@@@@@OP_LOADMQ@@@@@@@	
			@PRINTF@
			op_loadmq:
			ldr r0, =printf_op_loadmq
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			pop {r1}
			@@FAZENDO AC = MQ
			ldr r0, =ac
			ldr r1, =mq
			ldr r1, [r1]
			str r1, [r0]
			b fim_for
			@@@@@@@OP_LOADMQ@@@@@@@

			@@@@@@@@OP_LOADABS@@@@@
			@PRINTF@
			op_loadabs:
			ldr r0, =printf_op_loadabs
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			@@COLOCANDO MEMORY[ADDR] EM r2
			mov r5, #16					
			mul r0, r1, r5				@r0 = endereco(r1)*16
			ldr r1, =inicio_mem
			add r1, r1, r0				@r1 = inicio_mem + endereco*16 = &OP1
			add r1, #4					@r1 = inicio_mem + endereco*16 + 4 = &ARG OP 1
			ldr r2, [r1]				@r2 = ARG OP 1
			mov r2, r2, lsl #20			@r2 = (ARG OP 1) 00 000
			add r1, #4					@r1 = &(inicio_mem + endereco*16 + 4) = &OP 2
			ldr r3, [r1]				@r3 = (inicio_mem + endereco*16 + 4) = OP 2
			mov r3, r3, lsl #12			@r3 = 000 (OP 2) 000
			add r1, #4
			ldr r4, [r1]
			orr r2, r2, r3
			orr r2, r2, r4				@r2 = mem[addr]
			mov r3, r2, lsr #31			@r3 = digito mais significativo de mem[addr]
			cmp r3, #0					@vendo se ele eh neg ou nao
			@@SE NEG					
			mov r6, #0					
			subne r2, r6, r2			@se for neg, tornar, positivo
			ldr r0, =ac					@aqui ele ja deve estar certo
			str r2, [r0]
			b fim_for	
			@@@@@@@@OP_LOADABS@@@@@


			@@@@@@@@OP_LOADN@@@@@
			@PRINTF@
			op_loadn:
			ldr r0, =printf_op_loadn
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			@@FAZENDO AC = MEMORY[ADDR]
			mov r5, #16					
			mul r0, r1, r5				@r0 = endereco(r1)*16
			ldr r1, =inicio_mem
			add r1, r1, r0				@r1 = inicio_mem + endereco*16 = &OP1
			add r1, #4					@r1 = inicio_mem + endereco*16 + 4 = &ARG OP 1
			ldr r2, [r1]				@r2 = ARG OP 1
			mov r2, r2, lsl #20			@r2 = (ARG OP 1) 00 000
			add r1, #4					@r1 = &(inicio_mem + endereco*16 + 4) = &OP 2
			ldr r3, [r1]				@r3 = (inicio_mem + endereco*16 + 4) = OP 2
			mov r3, r3, lsl #12			@r3 = 000 (OP 2) 000
			add r1, #4
			ldr r4, [r1]
			orr r2, r2, r3
			orr r2, r2, r4				@r2 = valor a ter ac
			mov r4, #0
			sub r2, r4, r2				@r2 =  -valor
			ldr r1, =ac
			str r2, [r1]
			mov r0, #0
			b fim_for
			@@@@@@@@OP_LOADN@@@@@
			
			

			@@@@@@@@OP_STOR@@@@@ memory[addr] = ac;
			@PRINTF@
			op_stor:
			ldr r0, =printf_op_stor
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo
			@@FAZENDO MEMORY[ADDR] = AC
			ldr r1, =ac
			ldr r1, [r1]				@r0 = ac
			bl armazena_no_end			@r0 = end e r1 = o que armazenar
			b fim_for
			@@@@@@@@OP_STOR@@@@@
			
			@@@@@@@@OP_STORL@@@@@ 
			@PRINTF@
			op_storl:
			ldr r0, =printf_op_storl
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO MEMORY[ADDR] = FF 000 FF FFF, zerando onde ta 000 e mantendo o resto
			ldr r1, =inicio_mem
			mov r5, #16					
			mul r2, r0, r5				@r2 = endereco(r0)*16
			add r1, r1, r2				@r1 = endereco*16 + inicio_mem
			add r1, r1, #4
			push {r1}
			mov r0, #0x000
			str r0, [r1]				@armazenando 0, zerando
			@@Pegando os ultimos 12 bits de AC (3 hexas) e colocando no lugar que zerei
			ldr r0, =ac
			ldr r0, [r0]				@r0 = ac
			ldr r1, =fff
			ldr r1, [r1]
			and r2, r1, r0				@r2 = ultimos 3 hexas de ac
			@@agora vamos transportar esses 3 hexas para o zero de MEMORY[ADDR] = FF 000 FFF FF
			pop {r1}					@r1 = endereco onde devo armazenar
										@r0 = ac e r2 = o que devo armazenar
			str r2, [r1]
					@memory[addr] &= 0xFF000FFFFF;
					@memory[addr] ^= (ac & 0x0FFF) << 20;
			b fim_for
			@@@@@@@@OP_STORL@@@@@
			
			@@@@@@@@OP_STORR@@@@@ 
			@PRINTF@
			op_storr:
			ldr r0, =printf_op_storr
			ldr r1, =addr
			ldr r1, [r1]
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO MEMORY[ADDR] = FF FFF FF 000, zerando onde ta 000 e mantendo o resto
			ldr r1, =inicio_mem
			mov r5, #16					
			mul r2, r0, r5				@r2 = endereco(r0)*16
			add r1, r1, r2				@r1 = endereco*16 + inicio_mem
			add r1, r1, #12
			push {r1}
			mov r0, #0x000
			str r0, [r1]				@armazenando 0, zerando
			@@Pegando os ultimos 12 bits de AC (3 hexas) e colocando no lugar que zerei
			ldr r0, =ac
			ldr r0, [r0]				@r0 = ac
			ldr r1, =fff
			ldr r1, [r1]
			and r2, r1, r0				@r2 = ultimos 3 hexas de ac
			@@agora vamos transportar esses 3 hexas para o zero de MEMORY[ADDR] = FF 000 FFF FF
			pop {r1}					@r1 = endereco onde devo armazenar
										@r0 = ac e r2 = o que devo armazenar
			str r2, [r1]
					@memory[addr] &= 0xFF000FFFFF;
					@memory[addr] ^= (ac & 0x0FFF) << 20;
			b fim_for
			@@@@@@@@OP_STORR@@@@@
			
						@@@@@@@@OP_ADD@@@@@ 
			@PRINTF@
			op_add:
			ldr r0, =printf_op_add
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
			ldr r3, =ac
			mov r4, r3					@r4 = =ac
			ldr r3, [r3]				@r3 = valor de ac
			add r1, r1, r3				@r1 = mem[addr] + valor ac
			str r1, [r4]				@valor ac = mem[addr] + valor ac
			@bl armazena_no_end			@recebe em r0 o end e r1 o que armazenar
			
			
			b fim_for
			@@@@@@@@OP_ADD@@@@@
			
			
									@@@@@@@@OP_ADDABS@@@@@ 
			@PRINTF@
			op_addabs:
			ldr r0, =printf_op_add
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
			push {r1}
			mov r1, r1, lsr #31			@r3 = digito mais significativo de mem[addr]
			cmp r1, #0					@vendo se ele eh neg ou nao
			@@SE NEG					
			mov r6, #0					
			pop {r1}
			subne r1, r6, r1			@se for neg, tornar, positivo
			
			
			ldr r3, =ac
			mov r4, r3					@r4 = =ac
			ldr r3, [r3]				@r3 = valor de ac
			add r1, r1, r3				@r1 = mem[addr] + valor ac
			str r1, [r4]				@valor ac = mem[addr] + valor ac
			@bl armazena_no_end			@recebe em r0 o end e r1 o que armazenar
			
			
			b fim_for
			@@@@@@@@OP_ADDABS@@@@@
			
			
									@@@@@@@@OP_SUB@@@@@ 
			@PRINTF@ @ac = ac - memoria 
			op_sub:
			ldr r0, =printf_op_sub
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
			
			
			ldr r3, =ac
			mov r4, r3					@r4 = =ac
			ldr r3, [r3]				@r3 = valor de ac
			sub r1, r3, r1				@r1 = valor ac - mem[addr] 
			str r1, [r4]				@valor ac = mem[addr] + valor ac
			@bl armazena_no_end			@recebe em r0 o end e r1 o que armazenar
			
			
			b fim_for
			@@@@@@@@OP_SUB@@@@@
			
			
									@@@@@@@@OP_SUBABS@@@@@ 
			@PRINTF@
			op_subabs:
			ldr r0, =printf_op_subabs
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
			push {r1}
			mov r1, r1, lsr #31			@r3 = digito mais significativo de mem[addr]
			cmp r1, #0					@vendo se ele eh neg ou nao
			@@SE NEG					
			mov r6, #0					
			pop {r1}
			subne r1, r6, r1			@se for neg, tornar, positivo
			
			
			
			ldr r3, =ac
			mov r4, r3					@r4 = =ac
			ldr r3, [r3]				@r3 = valor de ac
			sub r1, r1, r3				@r1 = mem[addr] - valor ac
			str r1, [r4]				@valor ac = mem[addr] + valor ac
			@bl armazena_no_end			@recebe em r0 o end e r1 o que armazenar
			
			
			b fim_for
			@@@@@@@@OP_SUBABS@@@@@
			
			
									@@@@@@@@OP_MUL@@@@@ 
			@PRINTF@
			op_mul:
			ldr r0, =printf_op_mul
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
			push {r1}					@r1 = mem[addr]
			
			ldr r0, =mq
			ldr r0, [r0]				@r0 = valor mq
			ldr r1, =ffff
			ldr r1, [r1]				@r1 = 0xFFFF
			mov r7, r1					@r7 = 0xFFFF
			and r1, r1, r0				@r1 = 0xFFFF & mq = l0
			mov r0, r0, lsr #16			@r0 = valor mq >> 16 = h0
			pop {r2}					@r2= valor mem[addr]
			push {r2}
			and r2, r2, r7				@r2 = mem[addr] & 0xFFFF =  l1
			pop {r3}					@r3 = mem[addr]
			mov r3, r3, lsr #16			@r3 = mem[addr] >> 16 = h1
			mul r4, r1, r2				@r4 = l0 * l1
			mul r5, r1, r3				@r5 = l0 * h1
			mul r6, r2, r0				@r6 = l1 * h0
			add r5, r5, r6				@r5 = l0 * h1 + l1 * h0
			push {r5}					@vou usar para o ac
			mov r5, r5, lsl #16			@r5 = (l0 * h1 + l1 * h0) << 16
			add r5, r4, r5				@r5 = l0 * l1 + ((l0 * h1 + l1 * h0) << 16)
			ldr r7, =ffffffff
			ldr r7, [r7]
			and r5, r5, r7				@r5 =( (l0 * l1) + (l0 * l1 + ( (l0 * h1 + l1 * h0) << 16)) & 0xFFFFFFFF)
			ldr r7, =mq
			str r5, [r7]				
			mul r8, r0, r3				@r0 = h0 * h1
			mov r0, r8
			pop {r1}					@r1 = l0 * h1 + l1 * h0
			mov r1, r1, lsr #16			@r1 = (l0 * h1 + l1 * h0) >> 16
			add r0, r0, r1				@r0 = h0 * h1 + ((l0 * h1 + l1 * h0) >> 16)
			ldr r3, =ac
			str r0, [r3]
			
			
						@			int64_t l0, h0, l1, h1;
						@l0 = mq & 0xFFFF;
						@h0 = mq >> 16;
						@l1 = memory[addr] & 0xFFFF;
						@h1 = memory[addr] >> 16;
						@mq = ((l0 * l1) + ((l0 * h1 +
						@						l1 * h0) << 16)) & 0xFFFFFFFF;
						@ac = (h0 * h1) + ((l0 * h1 + l1 * h0) >> 16);
						@e a alegria flui!
			
			
			b fim_for
			@@@@@@@@OP_MUL@@@@@
			
			
									@@@@@@@@OP_DIV@@@@@ 
			@PRINTF@				@ac = ac / mem[addr]
			op_div:
			ldr r0, =printf_op_div
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
										
			cmp r1, #0					@se divisor = 0
			ldreq r0, =printf_div_zero	@dar erro
			push {r1}
			bleq printf
			pop {r1}
			cmp r1, #0
			beq sair
			ldr r0, =ac					@carrega em r0 o endereco de ac
			ldr r0, [r0]				@ r0 = ac
			mov r2, #0					@colocando quociente como 0 inicialmente
			bl dividir	
											@agora r2=quociente e r3=resto
			ldr r0, =mq
			str r2, [r0]				@mq = quociente
			ldr r0, =ac
			str r3, [r0]				@ac = resto
			
			
			b fim_for		
			dividir:					@r0 = dividendo r1 = divisor r2 = quociente r3 = resto
				cmp r0, r1  			@verificar se o numero e maior que r1 ou nao, se menor
				movlt r3, r0
				movlt pc, lr			@o resto esta em r0 e o quociente em r2, acabou
				sub r0, r0, r1			@subtrai de r0 r1
				add r2, r2, #1  		@adiciona ao quociente o valor 1
				bal dividir			@repete
				

			@@@@@@@@OP_DIV@@@@@
			
			
									@@@@@@@@OP_RSH@@@@@ 
			@PRINTF@
			op_rsh:
			ldr r0, =printf_op_rsh
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO MEMORY[ADDR] = FF FFF FF 000, zerando onde ta 000 e mantendo o resto
			ldr r0, =ac
			ldr r1, [r0]
			mov r1, r1, lsr #1
			str r1, [r0]
			
			
			b fim_for
			@@@@@@@@OP_RSH@@@@@
			
			
									@@@@@@@@OP_LSH@@@@@ 
			@PRINTF@
			op_lsh:
			ldr r0, =printf_op_lsh
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO MEMORY[ADDR] = FF FFF FF 000, zerando onde ta 000 e mantendo o resto
			ldr r0, =ac
			ldr r1, [r0]
			mov r1, r1, lsl #1
			str r1, [r0]
			
			
			b fim_for
			@@@@@@@@OP_LSH@@@@@
			
			
									@@@@@@@@OP_JUMPL@@@@@ 
			@PRINTF@
			op_jumpl:
			ldr r0, =printf_op_jumpl
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
										@r1 = mem[addr]
			
			ldr r0, =addr				@pc = addr
			ldr r0, [r0]
			ldr r1, =pc
			str r0, [r1]
			
			ldr r0, =first				@first = 0
			mov r1, #0
			str r1, [r0]
			
			ldr r0, =jump				@jump = 1
			mov r1, #1
			str r1, [r0]
			
			
			b fim_for
			@@@@@@@@OP_JUMPL@@@@@
			
			
									@@@@@@@@OP_JUMPR@@@@@ 
			@PRINTF@
			op_jumpr:
			ldr r0, =printf_op_jumpr
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
										@r1 = mem[addr]
			
			ldr r0, =addr				@pc = addr
			ldr r0, [r0]
			ldr r1, =pc
			str r0, [r1]
			
			ldr r0, =first				@first = 1
			mov r1, #1
			str r1, [r0]
			
			ldr r0, =jump				@jump = 1
			mov r1, #1
			str r1, [r0]
			
			
			b fim_for
			@@@@@@@@OP_JUMPR@@@@@
			
			
									@@@@@@@@OP_JUMPPL@@@@@ 
			@PRINTF@
			op_jumppl:
			ldr r0, =printf_op_jumppl
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
										@r1 = mem[addr]
			ldr r3, =ac
			ldr r3, [r3]
			mov r3, r3, lsr #31
			cmp r3, #0
			bne fim_for
			ldr r0, =addr				@pc = addr
			ldr r0, [r0]
			ldr r1, =pc
			str r0, [r1]
			
			ldr r0, =first				@first = 0
			mov r1, #0
			str r1, [r0]
			
			ldr r0, =jump				@jump = 1
			mov r1, #1
			str r1, [r0]
			
			
			b fim_for
			@@@@@@@@OP_JUMPPL@@@@@
			
			
									@@@@@@@@OP_JUMPPR@@@@@ 
			@PRINTF@
			op_jumppr:
			ldr r0, =printf_op_jumppr
			ldr r1, =addr
			ldr r1, [r1]						
			push {r1}
			bl printf
			pop {r1}
			@TESTANDO ENDERECO
			mov r0, r1					@r0 = endereco
			mov r7, r1					@preservando r1
			bl test_addr
			mov r1, r7
			mov r0, r1					@r0 = endereco de novo, valor dele
			@@FAZENDO 	ac += memory[addr];
			bl carrega_do_end 			@recebe em r0 o end e retorna r1 como mem[addr]
										@retorna em r2 o onde escrever para ir para mem[addr]
										@r1 = mem[addr]
			ldr r3, =ac
			ldr r3, [r3]
			mov r3, r3, lsr #31
			cmp r3, #0
			bne fim_for
			ldr r0, =addr				@pc = addr
			ldr r0, [r0]
			ldr r1, =pc
			str r0, [r1]
			
			ldr r0, =first				@first = 1
			mov r1, #1
			str r1, [r0]
			
			ldr r0, =jump				@jump = 1
			mov r1, #1
			str r1, [r0]
			
			
			b fim_for
			@@@@@@@@OP_JUMPPR@@@@@

	fim_for:
	@@@@@IMPRIMINDO @ Estado @@@@@
	ldr r0, =printf_state
	@r1 = ac & 0xFFFFFFFFFF, r2 = mq & 0xFFFFFFFFFF, r3 = pc & 0xFFFFFFFFFF
	ldr r1, =ac
	ldr r1, [r1]
	and r1, r1, #0xFFFFFFFF
	ldr r2, =mq
	ldr r2, [r2]
	and r2, r2, #0xFFFFFFFF
	ldr r3, =pc
	ldr r3, [r3]
	and r3, r3, #0xFFFFFFFF
	bl printf
	@@@@@FIM DA IMPRESSÃO@@@@@
	ldr r0, =j					@atualizando j
	ldr r1, [r0]
	add r1, r1, #1
	str r1, [r0]				@escrevendo j na memoria
	b comeco_for

	
		fim_switch:

		ldr r0, =jump				@pc = pc + 1
		ldr r0, [r0]
		cmp r0, #0
		ldreq r1, =pc
		ldreq r2, [r1]
		addeq r2, r2, #1
		streq r2, [r1]
	
		ldreq r1, =first			@first = 0
		moveq r2, #0
		streq r2, [r1]
	
		ldrne r1, =jump				@jump = 0
		movne r2, #0
		strne r2, [r1]
		b inicio_for
		
	pula_loop:
	


	ldr r0, =printf_mask		@carrego a mascara do printf
	ldr r1, =inicio_mem			@primeiro argumento do printf = address
	add r1, r1, #16
	ldr r1, [r1]
	ldr r2, =inicio_mem			@segundo argumento do printf = op1
	add r2, r2, #20
	ldr r2, [r2]
	ldr r3, =inicio_mem			@terceiro argumento do printf = arg op1
	add r3, r3, #24
	ldr r3, [r3]
	ldr r4, =inicio_mem			@quarto argumento do printf = op2
	add r4, r4, #28
	ldr r4, [r4]
	ldr r5, =addr					@quinto argumento do printf = arg op2
	@add r5, r5, #56
	ldr r5, [r5]
	push {r5}
	push {r4}
	bl printf
	pop {r4}
	pop {r5}
	mov r0, #0					@marking return as 0
	pop {pc}					@exiting




@@@@@ TESTANDO SE O ENDERECO E VALIDO @@@@@
test_addr:						@recebe em r0 o valor do endereco a testar
	push {lr}					@salvando lr
	push {r0-r10}				@salvando registradores
	cmp r0, #0					@comparo para ver se o end eh menor que 0
	movlt r1, r0				@se menor, coloco ele como arg do printf
	ldrlt r0, =printf_error_addr	
	bllt printf					@printo
	blt	sair					@saio
	cmp r0, #1024				@comparo o end com 1024, vejo se ele eh maior
	movge r1, r0				@se for maior, carrego o end no printf
	ldrge r0, =printf_error_addr@
	blge printf
	bge	sair
	pop {r0-r10}
	pop {pc}


		@@FAZENDO r1 = MEMORY[ADDR] e r2 = =inicio_mem+16*ADDR (onde escrever) // Recebe addr em r0 chamar com bl
	carrega_do_end:
	push {r3-r10}
	mov r5, #16					
	mul r0, r1, r5				@r0 = endereco(r1)*16
	ldr r1, =inicio_mem
	add r1, r1, r0				@r1 = inicio_mem + endereco*16 = &OP1
	push {r1}
	add r1, #4					@r1 = inicio_mem + endereco*16 + 4 = &ARG OP 1
	ldr r2, [r1]				@r2 = ARG OP 1
	mov r2, r2, lsl #20			@r2 = (ARG OP 1) 00 000
	add r1, #4					@r1 = &(inicio_mem + endereco*16 + 4) = &OP 2
	ldr r3, [r1]				@r3 = (inicio_mem + endereco*16 + 4) = OP 2
	mov r3, r3, lsl #12			@r3 = 000 (OP 2) 000
	add r1, #4
	ldr r4, [r1]
	orr r2, r2, r3
	orr r2, r2, r4				@r2 = valor a ter em r1
	mov r1, r2					@r1 = memory[addr]
	pop {r2}
	pop {r3-r10}
	mov pc, lr
	
	armazena_no_end:			@recebe em r0 o endereco e em r1 o que armazenar
	push {lr}
	bl test_addr					@testa o endereco 
	@@@@@ Salvando o OP 1 em inicio_mem[endereco] = 0 @@@@@
	push {r1-r3}
	mov r6, #16
	mul r4, r0, r6								@r4 = endereco * 16
	ldr r2, =inicio_mem							@coloca o endereco a ser escrito como inicio_mem
	add r2, r2, r4								@r2 = r2(=inicio_mem) + endereco*16 (r4)
	mov r3, #0
	str r3, [r2]								@salvando op1 em endereco*16
	pop {r1-r3}
		@@@@@ Salvando o ARG 1 em inicio_mem[endereco] @@@@@
	push {r1-r3}
	mov r6, #16
	mul r4, r0, r6								@r4 = endereco * 16
	ldr r2, =inicio_mem							@coloca o endereco a ser escrito como inicio_mem
	add r2, r2, r4								@r2 = r2(=inicio_mem) + endereco*16 (r4)
	add r2, r2, #4
	mov r1, r1, lsr #20
	ldr r3, =fff
	ldr r3, [r3]
	and r3, r3, r1
	str r3, [r2]								@salvando arg op1 em endereco*16
	pop {r1-r3}
	@@@@@ Salvando o OP 2 em inicio_mem[endereco+4] @@@@@
	push {r1-r3}
	mov r6, #16
	mul r4, r0, r6								@r4 = endereco * 16
	ldr r2, =inicio_mem							@coloca o endereco a ser escrito como inicio_mem
	add r2, r2, r4								@r1 = r1(=inicio_mem) + endereco*16 (r4)
	add r2, r2, #8								@armazena em =inicio_mem + end*16 + 4
	mov r1, r1, lsr #12
	mov r3, #0xFF
	and r3, r3, r1
	str r3, [r2]								@salvando op2 em endereco*16 + 8
	pop {r1-r3}
	@@@@@ Salvando o ARG OP 2 em inicio_mem[endereco+8] @@@@@
	push {r1-r3}
	mov r6, #16
	mul r4, r0, r6								@r4 = endereco * 16
	ldr r2, =inicio_mem							@coloca o endereco a ser escrito como inicio_mem
	add r2, r2, r4								@r1 = r1(=inicio_mem) + endereco*16 (r4)
	add r2, r2, #12								@armazena em =inicio_mem + end*16 + 12
	mov r1, r1, lsr #0
	ldr r3, =fff
	ldr r3, [r3]
	and r3, r3, r1
	str r3, [r2]								@salvando arg op2 em endereco*16
	pop {r1-r3}
	pop {pc}

	
	
	
sair:
	ldr r1, =lr
	ldr r1, [r1]
	mov lr, r1
	mov r0, #1					@retornando 1
	mov pc, lr					@saindo do programa

.data
	scanf_mask: .asciz "%x"		@mascara do scanf
	inicio_mem: .space 16384	@tamanho da memoria do programa de entrada
	printf_mask: .asciz "%x %x %x %x %x\n"	@mascara do printf
	numero_linhas: .space 4
	lr: .space 4
	fff: .word 0xFFF
	ffff: .word 0xFFFF
	ffffffff: .word 0xFFFFFFFF
	ac: .space 4
	mq: .space 4
	pc: .space 4
	jump: .space 4
	first: .space 4
	error: .space 4
	j: .space 4
	i: .space 4
	addr: .space 4
	inst: .space 4
	endereco: .space 4
	printf_init: .asciz "@ Estado inicial:\n"
	printf_state: .asciz "+ AC:  0x%010lx     MQ: 0x%010lx        PC: 0x%010lx\n--------------------------------------------------------------\n"
	printf_pc: .asciz "@ Executando instrucao no endereco %010x "
	printf_inst_esq: .asciz	"(instrucao a esquerda)\n"
	printf_inst_dir: .asciz "(instrucao a direita)\n"
	printf_op_load: .asciz "@ LOAD M(X), X = 0x%04x\n"
	printf_op_loadmqm: .asciz "@ LOAD MQ,M(X), X = 0x%04x\n"
	printf_op_loadmq: .asciz "@ LOAD MQ, X = 0x%04X\n"
	printf_op_loadabs: .asciz "@ LOAD |(M(X)|, X = 0x%04x\n"
	printf_op_loadn: .asciz "@ LOAD -(M(X)), X = 0x%04x\n"
	printf_op_stor: .asciz "@ STOR M(X), X = 0x%04x\n"
	printf_op_storl: .asciz "@ STOR M(X,8:19), X =0x%04x\n"
	printf_op_storr: .asciz "@ STOR M(X,28:39), X =0x%04x\n"
	
	printf_op_add: .asciz "@ ADD M(X), X = 0x%04X\n"
	printf_op_addabs: .asciz "@ ADD |M(X)|, X = 0x%04X\n"
	printf_op_sub: .asciz "@ SUB M(X), X = 0x%04X\n"
	printf_op_subabs: .asciz "@ SUB |M(X)|, X = 0x%04X\n"
	printf_op_mul: .asciz "@ MUL M(X), X = 0x%04X\n"
	printf_op_div: .asciz "@ DIV M(X), X = 0x%04X\n"
	printf_op_rsh: .asciz "@ RSH, X = 0x%04X\n"
	printf_op_lsh: .asciz "@ LSH, X = 0x%04X\n"
	printf_op_jumpl: .asciz "@ JUMP M(X,0:19), X = 0x%04X\n"
	printf_op_jumpr: .asciz "@ JUMP M(X,20:39), X = 0x%04X\n"
	printf_op_jumppl: .asciz "@ JUMP+ M(X,0:19), X = 0x%04X\n"
	printf_op_jumppr: .asciz "@ JUMP+ M(X,20:39), X = 0x%04X\n"
	
	printf_comeco: .asciz "\nIASIM: A simulacao esta comecando.\n\n"
	printf_error_addr: .asciz "IASIM: Erro! Endereco invalido de numero %04x.\n\nIASIM: A simulacao terminou.\n\n"
	printf_div_zero: .asciz "IASIM: Erro! Divisao por zero.\n\nIASIM: A simulacao terminou.\n\n"
	printf_error_inst_errada: .asciz "IASIM: Erro! Instrucao invalida com opcode %02x.\n\nIASIM: A simulacao terminou.\n\n"
	debug: .asciz "DEBUG\n"
	


  