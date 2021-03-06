#include "chess.cpp"

extern "C" {

  void new_game_w(gamestate *g)
  { *g = new_game(); }

  void print_move_w(move *m, char *buffer)
  { print_move(*m, buffer); }
  
  void print_fen_w(gamestate *g, char *buffer)
  { print_fen(*g, buffer); }

  void mkIterator_w(gamestate *g, iterator *i)
  { *i = mkIterator(*g); }

  void advance_iterator_w(gamestate *g, iterator *i, iterator *result)
  { *result = advance_iterator(*g, *i); }

  uint64_t perft_w(gamestate *g, int depth)
  { return perft(*g, depth); }

  void apply_move_w(gamestate *g, move *m, gamestate *result)
  { *result = apply_move(*g, *m); }

  int is_iterator_finished_w(iterator *i)
  { return is_iterator_finished(*i) ? 1 : 0; }

  void dereference_iterator_w(iterator *i, move *m)
  { *m = dereference_iterator(*i); }
  
};
