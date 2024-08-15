class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.
protected section.

  methods HEADER_OVSET_CREATE_ENTITY
    redefinition .
  methods HEADER_OVSET_DELETE_ENTITY
    redefinition .
  methods HEADER_OVSET_GET_ENTITY
    redefinition .
  methods HEADER_OVSET_GET_ENTITYSET
    redefinition .
  methods HEADER_OVSET_UPDATE_ENTITY
    redefinition .
  methods MENSAGEMSET_CREATE_ENTITY
    redefinition .
  methods MENSAGEMSET_DELETE_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITYSET
    redefinition .
  methods MENSAGEMSET_UPDATE_ENTITY
    redefinition .
  methods OV_ITEMSET_CREATE_ENTITY
    redefinition .
  methods OV_ITEMSET_DELETE_ENTITY
    redefinition .
  methods OV_ITEMSET_GET_ENTITY
    redefinition .
  methods OV_ITEMSET_UPDATE_ENTITY
    redefinition .
  methods OV_ITEMSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


  method HEADER_OVSET_CREATE_ENTITY.
  DATA: ld_lastid TYPE int4.
  DATA: ls_header    TYPE zovheader.

  DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
  ).

  MOVE-CORRESPONDING er_entity TO ls_header.

  ls_header-CRIACAO_DATA    = sy-datum.
  ls_header-CRIACAO_HORA    = sy-uzeit.
  ls_header-CRIACAO_USER    = sy-uname.

  SELECT SINGLE MAX( orderid )
    INTO ld_lastid
    FROM zovheader.

  ls_header-orderid = ld_lastid + 1.
  INSERT zovheader FROM ls_header.
  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao inserir ordem'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.

  " atualizando
  MOVE-CORRESPONDING ls_header TO er_entity.

  CONVERT
    DATE ls_header-CRIACAO_DATA
    TIME ls_header-CRIACAO_HORA
    INTO TIME STAMP er_entity-datacriacao
    TIME ZONE 'UTC'.
  endmethod.


  method HEADER_OVSET_DELETE_ENTITY.
  endmethod.


  method HEADER_OVSET_GET_ENTITY.
    DATA: ld_orderid TYPE zovheader-orderid.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_header     TYPE zovheader.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrderID'.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não informado'
      ).

      RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
    ld_orderid = ls_key_tab-value.

    SELECT SINGLE *
      INTO ls_header
      FROM zovheader
     WHERE orderid = ld_orderid.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_header TO er_entity.

      er_entity-criadopor = ls_header-criacao_user.

      CONVERT DATE ls_header-criacao_data
              TIME ls_header-criacao_hora
         INTO TIME STAMP er_entity-datacriacao
         TIME ZONE 'UTC'. "sy-zonlo.
    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não encontrado'
      ).

      RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  endmethod.


 method HEADER_OVSET_GET_ENTITYSET.
  DATA: lt_header       TYPE STANDARD TABLE OF zovheader.
  DATA: ls_header       TYPE zovheader.
  DATA: ls_entityset LIKE LINE OF et_entityset.

  DATA: lt_orderby   TYPE STANDARD TABLE OF string.
  DATA: ld_orderby   TYPE string.

  LOOP AT it_order INTO DATA(ls_order).
    TRANSLATE ls_order-property TO UPPER CASE.
    TRANSLATE ls_order-order TO UPPER CASE.
    IF ls_order-order = 'DESC'.
      ls_order-order = 'DESCENDING'.
    ELSE.
      ls_order-order = 'ASCENDING'.
    ENDIF.
    APPEND |{ ls_order-property } { ls_order-order }|
        TO lt_orderby.
  ENDLOOP.
  CONCATENATE LINES OF lt_orderby INTO ld_orderby SEPARATED BY ''.

  IF ld_orderby = ''.
    ld_orderby = 'OrderId ASCENDING'.
  ENDIF.

  SELECT *
    FROM zovheader
    WHERE (IV_FILTER_STRING)
    ORDER BY (ld_orderby)
    INTO TABLE @lt_header
    UP TO @is_paging-top ROWS
    OFFSET @is_paging-skip.

  LOOP AT lt_header INTO ls_header.
    CLEAR ls_entityset.
    MOVE-CORRESPONDING ls_header TO ls_entityset.

    ls_entityset-criadopor = ls_header-criacao_user.

    CONVERT DATE ls_header-criacao_data
            TIME ls_header-criacao_hora
       INTO TIME STAMP ls_entityset-datacriacao
       TIME ZONE sy-zonlo.

    APPEND ls_entityset TO et_entityset.
  ENDLOOP.
 endmethod.


  method HEADER_OVSET_UPDATE_ENTITY.
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    er_entity-orderid  = it_key_tab[ name = 'OrderID' ]-value.

    UPDATE zovheader
       SET clientid   = er_entity-clientid
           totalitens = er_entity-totalitens
           totalfrete = er_entity-totalfrete
           totalorder = er_entity-totalorder
           status     = er_entity-status
     WHERE orderid    = er_entity-orderid.

    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao atualizar item'
      ).

      RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  endmethod.


  method MENSAGEMSET_CREATE_ENTITY.
  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.
  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.
  endmethod.


  method OV_ITEMSET_CREATE_ENTITY.
    DATA: ls_item TYPE zovitem.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = er_entity
    ).

    MOVE-CORRESPONDING er_entity TO ls_item.

    IF er_entity-itemid = 0.
      SELECT SINGLE MAX( itemid )
        INTO er_entity-itemid
        FROM zovitem
       WHERE orderid = er_entity-orderid.

      er_entity-itemid = er_entity-itemid + 1.
    ENDIF.

    INSERT zovitem FROM ls_item.
    IF sy-subrc <> 0.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Erro ao inserir item'
      ).

      RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  endmethod.


  method OV_ITEMSET_DELETE_ENTITY.
  endmethod.


  method OV_ITEMSET_GET_ENTITY.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_item    TYPE zovitem.
    DATA: ld_error   TYPE flag.

    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " input
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrderID'.
    IF sy-subrc <> 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id da ordem não informado'
      ).
    ENDIF.
    ls_item-orderid = ls_key_tab-value.

    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'ItemId'.
    IF sy-subrc <> 0.
      ld_error = 'X'.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Id do item não informado'
      ).
    ENDIF.
    ls_item-itemid = ls_key_tab-value.

    IF ld_error = 'X'.
      RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    SELECT SINGLE *
      INTO ls_item
      FROM zovitem
     WHERE orderid = ls_item-orderid
       AND itemid  = ls_item-itemid.

    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_item TO er_entity.
    ELSE.
      lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Item não encontrado'
      ).

      RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.
  endmethod.


  method OV_ITEMSET_GET_ENTITYSET.
    DATA: ld_orderid       TYPE int4.
    DATA: lt_orderid_range TYPE RANGE OF int4.
    DATA: ls_orderid_range LIKE LINE OF lt_orderid_range.
    DATA: ls_key_tab       LIKE LINE OF it_key_tab.


    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'OrderID'.
    IF sy-subrc = 0.
      ld_orderid = ls_key_tab-value.

      CLEAR ls_orderid_range.
      ls_orderid_range-sign   = 'I'.
      ls_orderid_range-option = 'EQ'.
      ls_orderid_range-low    = ld_orderid.
      APPEND ls_orderid_range TO lt_orderid_range.
    ENDIF.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE et_entityset
      FROM zovitem
     WHERE orderid IN lt_orderid_range.
  endmethod.


  method OV_ITEMSET_UPDATE_ENTITY.
DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

  io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
  ).

  er_entity-orderid  = it_key_tab[ name = 'OrderID' ]-value.
  er_entity-itemid   = it_key_tab[ name = 'ItemId' ]-value.
  er_entity-precotol = er_entity-quantidade * er_entity-precouni.

  UPDATE zovitem
     SET material   = er_entity-material
         descricao  = er_entity-descricao
         quantidade = er_entity-quantidade
         precouni   = er_entity-precouni
         precotol   = er_entity-precotol
   WHERE orderid    = er_entity-orderid
     AND itemid     = er_entity-itemid.

  IF sy-subrc <> 0.
    lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Erro ao atualizar item'
    ).

    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
      EXPORTING
        message_container = lo_msg.
  ENDIF.
  endmethod.
ENDCLASS.
