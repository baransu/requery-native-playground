module Sql = Postgres.Sql;
module QueryBuilder = Postgres.QueryBuilder;

module Col = {
  type t('a) = {
    key: string,
    encode: 'a => QueryBuilder.expr,
    primary_key: bool,
    unique: bool,
    not_null: bool,
    check: option(QueryBuilder.expr),
  };

  let make =
      (
        ~key,
        ~encode,
        ~primary_key=false,
        ~unique=false,
        ~not_null=false,
        ~check=?,
        (),
      ) => {
    key,
    encode,
    primary_key,
    unique,
    not_null,
    check,
  };
};

module type Config = {
  type t;
  let tname: string;
  let columns: list(Col.t(t));
};

module type Table = {
  type t;
  let table_name: Sql.TableName.t;

  let create_table: unit => Sql.CreateTable.t;

  let insert_many: list(t) => Sql.Insert.t(Sql.Returning.t);
  let insert_one: t => Sql.Insert.t(Sql.Returning.t);

  let get_select_columns: Sql.Select.select => list(string);
  let get_insert_columns: Sql.Insert.t(Sql.Returning.t) => list(string);

  let get_exn: (string, list((string, string))) => string;
  let get: (string, list((string, string))) => option(string);
};

module Make = (Config: Config) : (Table with type t = Config.t) => {
  type t = Config.t;

  let table_name = QueryBuilder.tname(Config.tname);

  let create_table = () => {
    QueryBuilder.(
      Config.columns
      |> List.map((column: Col.t(t)) =>
           cdef(
             ~notNull=column.not_null,
             ~unique=column.unique,
             ~check=?column.check,
             ~primaryKey=column.primary_key,
             column.key,
             Types.text,
           )
         )
      |> createTable(table_name, ~ifNotExists=true)
    );
  };

  let to_string_row = row => {
    Config.columns
    |> List.map(column => Col.(column.key, column.encode(row)))
    |> RowEncode.stringRow;
  };

  let insert_many = (rows: list(t)) => {
    QueryBuilder.(
      RowEncode.(rows |> insertMany(to_string_row) |> into(table_name))
      |> returning(
           Config.columns
           |> List.map((column: Col.t(t)) =>
                Sql.Column.fromString(column.key)
              )
           |> Array.of_list,
         )
    );
  };

  let insert_one = (row: t) => {
    RowEncode.(row |> insertOne(to_string_row) |> into(table_name));
  };

  let get_insert_columns = (query: Sql.Insert.t(Sql.Returning.t)) =>
  switch (query.returning) {
    | None => []
    | Some(Columns(columns)) =>
      columns
      |> Array.map(Sql.Column.toTuple)
      |> Array.map(snd)
      |> Array.map(
           fun
           | Sql.Column.Named(name) => Some(name |> Sql.ColumnName.toString)
           | _ => None // TODO: when all get Sql.Column.All
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
    };

  let get_select_columns = (query: Sql.Select.select) =>
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

  let get = (column, row) =>
    row
    |> List.find_opt(((col, _)) => col == column)
    |> Belt.Option.map(_, snd);

  let get_exn = (column, row) => row |> get(column) |> Belt.Option.getExn;
};
