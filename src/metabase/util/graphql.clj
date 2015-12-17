(ns metabase.util.graphql
  "graphql schema"
  (:require [clojure.tools.logging :as log]))

(import 'graphql.schema.GraphQLObjectType)
(import 'graphql.schema.GraphQLSchema)

(import 'graphql.Scalars)
(import 'graphql.schema.GraphQLArgument)
(import 'graphql.schema.GraphQLFieldDefinition)
(import 'graphql.schema.GraphQLObjectType)
(import 'graphql.schema.GraphQLList)
(import 'graphql.schema.GraphQLType)
(import 'graphql.schema.DataFetcher)

(def keyword->type {:string Scalars/GraphQLString})

(defn- ->type
  [type]
  (cond
    (instance? GraphQLType type)               type
    (vector? type)                             (new GraphQLList (->type (first type)))
    (and (keyword? type) (keyword->type type)) (keyword->type type)
    :else (throw (Exception. (str "Unknown type: " type)))))

(defn- apply-rest
  [object items]
  (reduce (fn [obj item]
            (cond
              (instance? GraphQLArgument item)        (.argument obj item)
              (instance? DataFetcher item)            (.dataFetcher obj item)
              (instance? GraphQLFieldDefinition item) (.field obj item))
            obj)
          object
          items))

(defn schema
  [query]
  (-> (GraphQLSchema/newSchema)
    (.query query)
    (.build)))

(defn object
  [name & rest]
  (-> (GraphQLObjectType/newObject)
    (.name name)
    (apply-rest rest)
    (.build)))

(defn field
  [name type & rest]
  (-> (GraphQLFieldDefinition/newFieldDefinition)
      (.name name)
      (.type (->type type))
      (apply-rest rest)
      (.build)))

(defn argument
  [name type & rest]
  (-> (GraphQLArgument/newArgument)
      (.name name)
      (.type (->type type))
      (apply-rest rest)
      (.build)))

(defn listt
  [type]
  (new GraphQLList type))

(defn fetch
  [f]
  (reify DataFetcher
    (get [this env]
      (clojure.walk/stringify-keys (f env)))))

(defn env->where
  [env]
  (->> (.getArguments env)
       (filter (fn [[a b]] (not (nil? b))))
       (map (fn [[a b]] [:= (keyword a) b]))
       (into [:and])))
