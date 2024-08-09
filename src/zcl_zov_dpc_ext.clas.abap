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
  endmethod.


  method HEADER_OVSET_GET_ENTITYSET.
  endmethod.


  method HEADER_OVSET_UPDATE_ENTITY.
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
  endmethod.


  method OV_ITEMSET_GET_ENTITYSET.
  endmethod.


  method OV_ITEMSET_UPDATE_ENTITY.
  endmethod.
ENDCLASS.
