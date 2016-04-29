
# parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.8'

_lr_method = 'LALR'

_lr_signature = '6749BD370A31D74382B5D6562D456F92'
    
_lr_action_items = {'FALSE':([0,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[5,-24,-20,-23,-16,-19,-17,-18,-22,5,-4,5,5,-10,-11,-9,-12,5,-14,-5,-15,-13,]),'NIL':([0,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[2,-24,-20,-23,-16,-19,-17,-18,-22,2,-4,2,2,-10,-11,-9,-12,2,-14,-5,-15,-13,]),'TEXT':([0,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[4,-24,-20,-23,-16,-19,-17,-18,-22,4,-4,4,4,-10,-11,-9,-12,4,-14,-5,-15,-13,]),'SIMB':([0,1,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[6,15,-24,-20,-23,-16,-19,-17,-18,-22,6,-4,6,6,-10,-11,-9,-12,6,-14,-5,-15,-13,]),'NUM':([0,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[7,-24,-20,-23,-16,-19,-17,-18,-22,7,-4,7,7,-10,-11,-9,-12,7,-14,-5,-15,-13,]),'LET':([0,1,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[12,16,-24,-20,-23,-16,-19,-17,-18,-22,12,-4,12,12,-10,-11,-9,-12,12,-14,-5,-15,-13,]),'LPAREN':([0,2,4,5,6,7,9,12,13,14,15,16,17,18,19,20,22,23,24,25,28,30,31,32,34,],[1,-24,-20,-23,-16,-19,18,-17,-18,-22,1,25,-4,1,1,-10,-11,-9,-12,1,-14,-5,1,-15,-13,]),'QUOTE':([0,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[9,-24,-20,-23,-16,-19,-17,-18,-22,9,-4,9,9,-10,-11,-9,-12,9,-14,-5,-15,-13,]),'RPAREN':([2,4,5,6,7,12,13,14,15,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,],[-24,-20,-23,-16,-19,-17,-18,-22,-8,-4,-8,-8,-10,28,-11,-9,-7,-8,30,-6,-14,31,-5,32,-15,34,-13,]),'TRUE':([0,2,4,5,6,7,12,13,14,15,17,18,19,20,22,23,24,25,28,30,32,34,],[14,-24,-20,-23,-16,-19,-17,-18,-22,14,-4,14,14,-10,-11,-9,-12,14,-14,-5,-15,-13,]),'$end':([0,2,3,4,5,6,7,8,10,11,12,13,14,17,28,30,32,34,],[-21,-24,-2,-20,-23,-16,-19,-3,0,-1,-17,-18,-22,-4,-14,-5,-15,-13,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'quoted_list':([0,15,18,19,25,],[3,20,20,20,20,]),'items':([15,18,19,25,],[21,26,27,29,]),'list':([9,],[17,]),'item':([15,18,19,25,],[19,19,19,19,]),'bool':([0,15,18,19,25,],[13,13,13,13,13,]),'exp':([0,],[10,]),'atom':([0,15,18,19,25,],[11,23,23,23,23,]),'call':([0,15,18,19,25,31,],[8,22,22,22,22,33,]),'empty':([15,18,19,25,],[24,24,24,24,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> exp","S'",1,None,None,None),
  ('exp -> atom','exp',1,'p_exp_atom','yacc.py',140),
  ('exp -> quoted_list','exp',1,'p_exp_qlist','yacc.py',144),
  ('exp -> call','exp',1,'p_exp_call','yacc.py',148),
  ('quoted_list -> QUOTE list','quoted_list',2,'p_quoted_list','yacc.py',152),
  ('list -> LPAREN items RPAREN','list',3,'p_list','yacc.py',156),
  ('items -> item items','items',2,'p_items','yacc.py',160),
  ('items -> empty','items',1,'p_items_empty','yacc.py',164),
  ('empty -> <empty>','empty',0,'p_empty','yacc.py',168),
  ('item -> atom','item',1,'p_item_atom','yacc.py',172),
  ('item -> quoted_list','item',1,'p_item_list','yacc.py',180),
  ('item -> call','item',1,'p_item_call','yacc.py',184),
  ('item -> empty','item',1,'p_item_empty','yacc.py',188),
  ('call -> LPAREN LET LPAREN items RPAREN call RPAREN','call',7,'p_call','yacc.py',199),
  ('call -> LPAREN SIMB items RPAREN','call',4,'p_callOG','yacc.py',214),
  ('call -> LPAREN LET LPAREN items RPAREN RPAREN','call',6,'p_callLetTwo','yacc.py',228),
  ('atom -> SIMB','atom',1,'p_atom_simbol','yacc.py',235),
  ('atom -> LET','atom',1,'p_atom_let','yacc.py',239),
  ('atom -> bool','atom',1,'p_atom_bool','yacc.py',243),
  ('atom -> NUM','atom',1,'p_atom_num','yacc.py',247),
  ('atom -> TEXT','atom',1,'p_atom_word','yacc.py',251),
  ('atom -> <empty>','atom',0,'p_atom_empty','yacc.py',255),
  ('bool -> TRUE','bool',1,'p_true','yacc.py',259),
  ('bool -> FALSE','bool',1,'p_false','yacc.py',263),
  ('atom -> NIL','atom',1,'p_nil','yacc.py',267),
]
