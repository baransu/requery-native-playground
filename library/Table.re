module type Config = {
  type t;
  let tname: string;
  let columns: list((string, t => QueryBuilder.expr));
};

module type Table = {
  include Config;
  let insert_many: (list(t)) => Sql.Insert.t(Postgres.Sql.Returning.t);
  let get_columns: Sql.Select.select => list(string);
};

module Make = (Config: Config) : (Table with type t = Config.t) => {
  include Config;

  let table_name = QueryBuilder.tname(Config.tname);

  let insert_many = (rows: list(t)) => {
    RowEncode.(
      rows
      |> insertMany(user =>
           Config.columns
           |> List.map(((col, fn)) => (col, fn(user)))
           |> stringRow
         )
      |> into(table_name)
    );
  };

  let get_columns = (query: Sql.Select.select) =>
    switch (query.select) {
    | Select({selections, _}) =>
      selections
      |> Array.map(Sql.Aliased.toTuple)
      |> Array.map(fst)
      |> Array.map(
           fun
           | Sql.Expression.Atom(Sql.Expression.Column(column)) =>
             switch (column |> Sql.Column.toTuple) {
             | (_, Sql.Column.Named(name)) =>
               Some(name |> Sql.ColumnName.toString)
             | _ => None // TODO: when all get Sql.Column.All
             }
           | _ => None,
         )
      |> Array.fold_right(
           (item, acc) =>
             switch (item) {
             | None => acc
             | Some(item) => [item, ...acc]
             },
           _,
           [],
         )
    | Union(_, _) => []
    | UnionAll(_, _) => []
    };
};
