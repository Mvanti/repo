!
    MODULE bdlista
!
    INTERFACE inicialise
      MODULE PROCEDURE inicialise_nocc
      MODULE PROCEDURE inicialise_aresta
      MODULE PROCEDURE inicialise_arem
    END INTERFACE
!
    INTERFACE novelist
      MODULE PROCEDURE novelist_nocc
      MODULE PROCEDURE novelist_aresta
      MODULE PROCEDURE novelist_arem
    END INTERFACE
!
    INTERFACE copilist
      MODULE PROCEDURE copilist_nocc
      MODULE PROCEDURE copilist_aresta
      MODULE PROCEDURE copilist_arem
    END INTERFACE
!
    INTERFACE RecupLista
      MODULE PROCEDURE RecupLista_aresta
    END INTERFACE
!
    INTERFACE destlist
        MODULE PROCEDURE destlist_nocc
        MODULE PROCEDURE destlist_aresta
        MODULE PROCEDURE destlist_arem
    END INTERFACE
!
   TYPE nocc
      PRIVATE
      INTEGER               :: nno
      REAL(8)               :: valor
      TYPE(nocc),POINTER    :: prox
    END TYPE nocc
!
    TYPE aresta
        PRIVATE
        INTEGER             :: nno(2)
        TYPE(aresta),POINTER:: prox,ante
    END TYPE
!
    TYPE lnocc
      PRIVATE
      TYPE(nocc),POINTER    :: LISTA,CORRENTE
    END TYPE
!
    TYPE laresta
      PRIVATE
      TYPE(aresta),POINTER  :: lista,corrente,lfixo
    END TYPE
!
    TYPE arem
        PRIVATE
        INTEGER             ::  aresta(3)
        TYPE(arem),POINTER  ::  prox
    END TYPE
!
    TYPE larem
        PRIVATE
        TYPE(arem),POINTER  ::  LISTA,CORRENTE
    END TYPE
!
    CONTAINS
!
!	INICIALISE : Inicializa uma pilha ou lista.
!	-------------------------------------------
!
    SUBROUTINE inicialise_nocc ( listen )
    IMPLICIT NONE
    TYPE(lnocc) :: listen
!
    NULLIFY(listen%lista)
    RETURN
    END SUBROUTINE inicialise_nocc
!
    SUBROUTINE inicialise_aresta ( listen )
    IMPLICIT NONE
    TYPE(laresta) :: listen
!
    NULLIFY(listen%lista)
    NULLIFY(listen%lfixo)
    RETURN
    END SUBROUTINE inicialise_aresta
!
    SUBROUTINE inicialise_arem ( listen )
    IMPLICIT NONE
    TYPE(larem) :: listen
!
    NULLIFY(listen%lista)
    RETURN
    END SUBROUTINE inicialise_arem
!
!	NOVELIST : Adiciona um elemento à lista
!	--------------------------------------
!
    SUBROUTINE novelist_nocc (numno,nval , listen)
    IMPLICIT  NONE
    INTEGER         ::  numno
    REAL(8)         ::  nval
    TYPE (lnocc)    ::  listen
!
    INTEGER         ::   ier
!
!   Atribui a lista.
    ALLOCATE(listen%corrente,STAT=ier)
    IF (ier > 0) THEN
      WRITE(*,*)'ERRO: ad_membro'
      WRITE(*,*)'Falha:Não criou novo nó'
      RETURN
    END IF
!
    listen%corrente%valor = nval
    listen%corrente%nno = numno
    listen%corrente%prox => listen%lista
    listen%lista => listen%corrente
!
    RETURN
    END SUBROUTINE novelist_nocc
!
    SUBROUTINE novelist_aresta (numno , listen)
    IMPLICIT  NONE
    INTEGER       :: numno(2)
    TYPE (laresta):: listen
!
    INTEGER       :: ier
!
!   Atribui a lista.
    ALLOCATE(listen%corrente,STAT=ier)
    IF (ier > 0) THEN
      WRITE(*,*)'ERRO: ad_membro'
      WRITE(*,*)'Falha:Não criou novo elemento ARESTA'
      RETURN
    END IF
!
    IF (.NOT. ASSOCIATED(listen%lfixo))THEN
        NULLIFY(listen%corrente%prox)
        listen%lfixo=>listen%corrente
    ELSE
        listen%lista%ante=>listen%corrente
    END IF
!
    listen%corrente%nno = numno
    listen%corrente%prox => listen%lista
    NULLIFY(listen%corrente%ante)
    listen%lista => listen%corrente
!
    RETURN
    END SUBROUTINE novelist_aresta
!
    SUBROUTINE novelist_arem (numar , listen)
    IMPLICIT NONE
    INTEGER         :: numar(3)
    TYPE(larem)     :: listen
!
    INTEGER         :: ier
!
!   Atribui a lista.
    ALLOCATE(listen%corrente,STAT=ier)
    IF (ier > 0) THEN
      WRITE(*,*)'ERRO: ad_membro'
      WRITE(*,*)'Falha:Não criou novo elemento AREM'
      RETURN
    END IF
!
    listen%corrente%aresta = numar
    listen%corrente%prox => listen%lista
    listen%lista => listen%corrente
!
    RETURN
    END SUBROUTINE novelist_arem
!
!	RecupLista	:	Recupera o ID-ésimo dado da lista
!	-------------------------------------------------
    SUBROUTINE RecupLista_aresta(id, listen , lare)
!
    IMPLICIT NONE
    INTEGER,INTENT(IN)  :: id
    TYPE(laresta)       :: listen
    INTEGER,INTENT(OUT) :: lare(2)
!
    INTEGER             :: i
!
    i=1
    listen%corrente=>listen%lfixo
!
!	Percorre a lista em busca de id
    DO
       IF (i<id)THEN
            listen%corrente=>listen%corrente%ante
            IF(.NOT. ASSOCIATED (listen%corrente))THEN
            WRITE(*,*)'RecupLista_aresta:ID inexistente'
              RETURN
            END IF
            i=i+1
            CYCLE
        ELSE
            EXIT
        END IF
    END DO
!
    lare=listen%corrente%nno
    listen%corrente=>listen%lista
!
    END SUBROUTINE RecupLista_aresta
!
!	COPILIST : Copia os dados da lista em tabelas
!	---------------------------------------------
    SUBROUTINE copilist_nocc (listen , estad,lno,lval)
!
    IMPLICIT NONE
    TYPE(lnocc)         :: listen
    LOGICAL,INTENT(OUT) :: estad
    INTEGER,INTENT(OUT) :: lno(:)
    REAL(8),INTENT(OUT) :: lval(:)
!
    INTEGER             :: i,numoc
!
    estad=.true.
    numoc=SIZE(lno)
    i=0
    listen%corrente=>listen%lista
    DO
      IF (.NOT. ASSOCIATED(listen%corrente))EXIT
      IF((numoc-i) <= 0)THEN
        estad=.false.
        RETURN
      END IF
      lval(numoc-i)=listen%corrente%valor
      lno(numoc-i)=listen%corrente%nno
      listen%corrente => listen%corrente%prox
      i=i+1
    END DO
    listen%corrente=>listen%lista
!
    END SUBROUTINE copilist_nocc
!
    SUBROUTINE copilist_aresta (listen , estad,lars)
!
    IMPLICIT NONE
    TYPE(laresta)       :: listen
    LOGICAL,INTENT(OUT) :: estad
    INTEGER,INTENT(OUT) :: lars(:,:)
!
    INTEGER             :: i,numar
!
    estad=.true.
    numar=SIZE(lars,1)
    i=0
    listen%corrente=>listen%lista
    DO
      IF (.NOT. ASSOCIATED(listen%corrente))EXIT
      IF((numar-i) <= 0)THEN
        estad=.false.
        RETURN
      END IF
      lars(numar-i,:)=listen%corrente%nno
      listen%corrente => listen%corrente%prox
      i=i+1
    END DO
    listen%corrente=>listen%lista
!
    END SUBROUTINE copilist_aresta
!
    SUBROUTINE copilist_arem (listen , estad,larel)
!
    IMPLICIT NONE
    TYPE(larem)         :: listen
    LOGICAL,INTENT(OUT) :: estad
    INTEGER,INTENT(OUT) :: larel(:,:)
!
    INTEGER             :: i,numel
!
    estad=.true.
    numel=SIZE(larel,1)
    i=0
    listen%corrente=>listen%lista
    DO
      IF (.NOT. ASSOCIATED(listen%corrente))EXIT
      IF((numel-i) <= 0)THEN
        estad=.false.
        RETURN
      END IF
      larel(numel-i,:)=listen%corrente%aresta
      listen%corrente => listen%corrente%prox
      i=i+1
    END DO
    listen%corrente=>listen%lista
!
    END SUBROUTINE copilist_arem
!
!	DESTLIST elimina a lista
!	------------------------
    SUBROUTINE destlist_nocc( listen )
!
    IMPLICIT NONE
    TYPE(lnocc) ::  listen
!
!   Atravessa a lista e desaloca cada no
    listen%corrente=>listen%lista
    DO
      IF (.NOT. ASSOCIATED(listen%corrente))EXIT
      listen%lista => listen%corrente%prox
      DEALLOCATE(listen%corrente)
      listen%corrente=>listen%lista
    END DO
!
    NULLIFY(listen%lista)
    RETURN
    END SUBROUTINE destlist_nocc
!
    SUBROUTINE destlist_aresta( listen )
!
    IMPLICIT NONE
    TYPE(laresta) ::  listen
!
!   Atravessa a lista e desaloca cada no
    listen%corrente=>listen%lista
    DO
      IF (.NOT. ASSOCIATED(listen%corrente))EXIT
      listen%lista => listen%corrente%prox
      DEALLOCATE(listen%corrente)
      listen%corrente=>listen%lista
    END DO
!
    NULLIFY(listen%lista)
    NULLIFY(listen%lfixo)
    RETURN
    END SUBROUTINE destlist_aresta
!
    SUBROUTINE destlist_arem( listen )
!
    IMPLICIT NONE
    TYPE(larem) ::  listen
!
!   Atravessa a lista e desaloca cada no
    listen%corrente=>listen%lista
    DO
      IF (.NOT. ASSOCIATED(listen%corrente))EXIT
      listen%lista => listen%corrente%prox
      DEALLOCATE(listen%corrente)
      listen%corrente=>listen%lista
    END DO
!
    NULLIFY(listen%lista)
    RETURN
    END SUBROUTINE destlist_arem
!
    END MODULE
