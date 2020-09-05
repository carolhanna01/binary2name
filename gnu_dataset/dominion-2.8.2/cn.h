typedef struct comp_params {
  int hos;			/* percent chance of lowering status */
  int dip_chan;			/* percent chance of changing status */
  int agg;			/* 'weight' for enemy sectors */
  int exp;			/* How thinly the cn spreads. */
  int mil;
  int army_size;
  int min_size;
  int merge_size_pct;
  int pat;
  int pat_size_pct;
} Scomp_params;

struct tmp_map{
  int mvcost,mvleft;
};
struct desire{
  int base,final,patrol,inter;
};
