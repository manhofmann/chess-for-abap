PROCESS BEFORE OUTPUT.

  MODULE status_9000.

  CALL SUBSCREEN subscreen_board
   INCLUDING sy-repid zchess_gui_dynpro_fields-board_subscreen.

PROCESS AFTER INPUT.
  MODULE user_command_9000.
