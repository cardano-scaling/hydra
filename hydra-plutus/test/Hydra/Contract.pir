(program
  (let
    (rec)
    (datatypebind
      (datatype
        (tyvardecl List (fun (type) (type)))
        (tyvardecl a (type))
        Nil_match
        (vardecl Nil [List a]) (vardecl Cons (fun a (fun [List a] [List a])))
      )
    )
    (let
      (nonrec)
      (datatypebind
        (datatype
          (tyvardecl HeadParameters (type))

          HeadParameters_match
          (vardecl HeadParameters (fun [List (con bytestring)] HeadParameters))
        )
      )
      (datatypebind
        (datatype (tyvardecl UTXO (type))  UTXO_match (vardecl UTXO UTXO))
      )
      (datatypebind
        (datatype
          (tyvardecl MultiSignature (type))

          MultiSignature_match
          (vardecl MultiSignature MultiSignature)
        )
      )
      (datatypebind
        (datatype
          (tyvardecl Transaction (type))

          Transaction_match
          (vardecl Transaction Transaction)
        )
      )
      (datatypebind
        (datatype
          (tyvardecl TransactionObject (type))

          TransactionObject_match
          (vardecl
            TransactionObject
            (fun MultiSignature (fun Transaction TransactionObject))
          )
        )
      )
      (datatypebind
        (datatype
          (tyvardecl Xi (type))

          Xi_match
          (vardecl
            Xi
            (fun UTXO (fun (con integer) (fun MultiSignature (fun [List TransactionObject] Xi))))
          )
        )
      )
      (datatypebind
        (datatype
          (tyvardecl HydraInput (type))

          HydraInput_match
          (vardecl Close (fun Xi HydraInput))
          (vardecl CollectCom HydraInput)
          (vardecl Init (fun HeadParameters HydraInput))
        )
      )
      (datatypebind
        (datatype
          (tyvardecl Eta (type))

          Eta_match
          (vardecl
            Eta (fun UTXO (fun (con integer) (fun [List Transaction] Eta)))
          )
        )
      )
      (datatypebind
        (datatype
          (tyvardecl MultisigPublicKey (type))

          MultisigPublicKey_match
          (vardecl
            MultisigPublicKey (fun [List (con bytestring)] MultisigPublicKey)
          )
        )
      )
      (datatypebind
        (datatype
          (tyvardecl OpenState (type))

          OpenState_match
          (vardecl OpenState (fun MultisigPublicKey (fun Eta OpenState)))
        )
      )
      (datatypebind
        (datatype
          (tyvardecl HydraState (type))

          HydraState_match
          (vardecl Closed HydraState)
          (vardecl Collecting HydraState)
          (vardecl Initial HydraState)
          (vardecl Open (fun OpenState HydraState))
        )
      )
      (datatypebind
        (datatype
          (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
          (tyvardecl a (type)) (tyvardecl b (type))
          Tuple2_match
          (vardecl Tuple2 (fun a (fun b [[Tuple2 a] b])))
        )
      )
      (let
        (rec)
        (datatypebind
          (datatype
            (tyvardecl Data (type))

            Data_match
            (vardecl B (fun (con bytestring) Data))
            (vardecl Constr (fun (con integer) (fun [List Data] Data)))
            (vardecl I (fun (con integer) Data))
            (vardecl List (fun [List Data] Data))
            (vardecl Map (fun [List [[Tuple2 Data] Data]] Data))
          )
        )
        (let
          (nonrec)
          (datatypebind
            (datatype
              (tyvardecl Extended (fun (type) (type)))
              (tyvardecl a (type))
              Extended_match
              (vardecl Finite (fun a [Extended a]))
              (vardecl NegInf [Extended a])
              (vardecl PosInf [Extended a])
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Bool (type))

              Bool_match
              (vardecl True Bool) (vardecl False Bool)
            )
          )
          (datatypebind
            (datatype
              (tyvardecl LowerBound (fun (type) (type)))
              (tyvardecl a (type))
              LowerBound_match
              (vardecl LowerBound (fun [Extended a] (fun Bool [LowerBound a])))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl UpperBound (fun (type) (type)))
              (tyvardecl a (type))
              UpperBound_match
              (vardecl UpperBound (fun [Extended a] (fun Bool [UpperBound a])))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Interval (fun (type) (type)))
              (tyvardecl a (type))
              Interval_match
              (vardecl
                Interval (fun [LowerBound a] (fun [UpperBound a] [Interval a]))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Tuple3 (fun (type) (fun (type) (fun (type) (type)))))
              (tyvardecl a (type)) (tyvardecl b (type)) (tyvardecl c (type))
              Tuple3_match
              (vardecl Tuple3 (fun a (fun b (fun c [[[Tuple3 a] b] c]))))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxOutRef (type))

              TxOutRef_match
              (vardecl
                TxOutRef (fun (con bytestring) (fun (con integer) TxOutRef))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Maybe (fun (type) (type)))
              (tyvardecl a (type))
              Maybe_match
              (vardecl Just (fun a [Maybe a])) (vardecl Nothing [Maybe a])
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxInInfo (type))

              TxInInfo_match
              (vardecl
                TxInInfo
                (fun TxOutRef (fun [Maybe [[[Tuple3 (con bytestring)] (con bytestring)] (con bytestring)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] TxInInfo)))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Address (type))

              Address_match
              (vardecl PubKeyAddress (fun (con bytestring) Address))
              (vardecl ScriptAddress (fun (con bytestring) Address))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxOutType (type))

              TxOutType_match
              (vardecl PayToPubKey TxOutType)
              (vardecl PayToScript (fun (con bytestring) TxOutType))
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxOut (type))

              TxOut_match
              (vardecl
                TxOut
                (fun Address (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun TxOutType TxOut)))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl TxInfo (type))

              TxInfo_match
              (vardecl
                TxInfo
                (fun [List TxInInfo] (fun [List TxOut] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [Interval (con integer)] (fun [List (con bytestring)] (fun [List (con bytestring)] (fun [List [[Tuple2 (con bytestring)] Data]] (fun (con bytestring) TxInfo)))))))))
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl ValidatorCtx (type))

              ValidatorCtx_match
              (vardecl
                ValidatorCtx (fun TxInfo (fun (con integer) ValidatorCtx))
              )
            )
          )
          (termbind
            (strict)
            (vardecl equalsInteger (fun (con integer) (fun (con integer) Bool)))
            (lam
              arg
              (con integer)
              (lam
                arg
                (con integer)
                [
                  (lam
                    b
                    (con bool)
                    [ [ [ { (builtin ifThenElse) Bool } b ] True ] False ]
                  )
                  [ [ (builtin equalsInteger) arg ] arg ]
                ]
              )
            )
          )
          (datatypebind
            (datatype
              (tyvardecl Monoid (fun (type) (type)))
              (tyvardecl a (type))
              Monoid_match
              (vardecl
                CConsMonoid
                (fun [(lam a (type) (fun a (fun a a))) a] (fun a [Monoid a]))
              )
            )
          )
          (termbind
            (strict)
            (vardecl
              p1Monoid
              (all a (type) (fun [Monoid a] [(lam a (type) (fun a (fun a a))) a]))
            )
            (abs
              a
              (type)
              (lam
                v
                [Monoid a]
                [
                  {
                    [ { Monoid_match a } v ]
                    [(lam a (type) (fun a (fun a a))) a]
                  }
                  (lam v [(lam a (type) (fun a (fun a a))) a] (lam v a v))
                ]
              )
            )
          )
          (termbind
            (strict)
            (vardecl mempty (all a (type) (fun [Monoid a] a)))
            (abs
              a
              (type)
              (lam
                v
                [Monoid a]
                [
                  { [ { Monoid_match a } v ] a }
                  (lam v [(lam a (type) (fun a (fun a a))) a] (lam v a v))
                ]
              )
            )
          )
          (datatypebind
            (datatype (tyvardecl Unit (type))  Unit_match (vardecl Unit Unit))
          )
          (let
            (rec)
            (termbind
              (nonstrict)
              (vardecl
                fFoldableNil_cfoldMap
                (all m (type) (all a (type) (fun [Monoid m] (fun (fun a m) (fun [List a] m)))))
              )
              (abs
                m
                (type)
                (abs
                  a
                  (type)
                  (lam
                    dMonoid
                    [Monoid m]
                    (let
                      (nonrec)
                      (termbind
                        (nonstrict)
                        (vardecl dSemigroup [(lam a (type) (fun a (fun a a))) m]
                        )
                        [ { p1Monoid m } dMonoid ]
                      )
                      (lam
                        ds
                        (fun a m)
                        (lam
                          ds
                          [List a]
                          [
                            [
                              [
                                { [ { Nil_match a } ds ] (fun Unit m) }
                                (lam thunk Unit [ { mempty m } dMonoid ])
                              ]
                              (lam
                                x
                                a
                                (lam
                                  xs
                                  [List a]
                                  (lam
                                    thunk
                                    Unit
                                    [
                                      [ dSemigroup [ ds x ] ]
                                      [
                                        [
                                          [
                                            { { fFoldableNil_cfoldMap m } a }
                                            dMonoid
                                          ]
                                          ds
                                        ]
                                        xs
                                      ]
                                    ]
                                  )
                                )
                              )
                            ]
                            Unit
                          ]
                        )
                      )
                    )
                  )
                )
              )
            )
            (let
              (nonrec)
              (datatypebind
                (datatype
                  (tyvardecl MultiplicativeMonoid (fun (type) (type)))
                  (tyvardecl a (type))
                  MultiplicativeMonoid_match
                  (vardecl
                    CConsMultiplicativeMonoid
                    (fun [(lam a (type) (fun a (fun a a))) a] (fun a [MultiplicativeMonoid a]))
                  )
                )
              )
              (termbind
                (strict)
                (vardecl
                  p1MultiplicativeMonoid
                  (all a (type) (fun [MultiplicativeMonoid a] [(lam a (type) (fun a (fun a a))) a]))
                )
                (abs
                  a
                  (type)
                  (lam
                    v
                    [MultiplicativeMonoid a]
                    [
                      {
                        [ { MultiplicativeMonoid_match a } v ]
                        [(lam a (type) (fun a (fun a a))) a]
                      }
                      (lam v [(lam a (type) (fun a (fun a a))) a] (lam v a v))
                    ]
                  )
                )
              )
              (termbind
                (strict)
                (vardecl one (all a (type) (fun [MultiplicativeMonoid a] a)))
                (abs
                  a
                  (type)
                  (lam
                    v
                    [MultiplicativeMonoid a]
                    [
                      { [ { MultiplicativeMonoid_match a } v ] a }
                      (lam v [(lam a (type) (fun a (fun a a))) a] (lam v a v))
                    ]
                  )
                )
              )
              (termbind
                (strict)
                (vardecl
                  fMonoidProduct
                  (all a (type) (fun [MultiplicativeMonoid a] [Monoid [(lam a (type) a) a]]))
                )
                (abs
                  a
                  (type)
                  (lam
                    v
                    [MultiplicativeMonoid a]
                    [
                      [
                        { CConsMonoid [(lam a (type) a) a] }
                        (lam
                          eta
                          [(lam a (type) a) a]
                          (lam
                            eta
                            [(lam a (type) a) a]
                            [ [ [ { p1MultiplicativeMonoid a } v ] eta ] eta ]
                          )
                        )
                      ]
                      [ { one a } v ]
                    ]
                  )
                )
              )
              (termbind
                (strict)
                (vardecl bad_name (fun Bool (fun Bool Bool)))
                (lam
                  ds
                  Bool
                  (lam
                    x
                    Bool
                    [
                      [
                        [
                          { [ Bool_match ds ] (fun Unit Bool) }
                          (lam thunk Unit x)
                        ]
                        (lam thunk Unit False)
                      ]
                      Unit
                    ]
                  )
                )
              )
              (termbind
                (nonstrict)
                (vardecl fMultiplicativeMonoidBool [MultiplicativeMonoid Bool])
                [ [ { CConsMultiplicativeMonoid Bool } bad_name ] True ]
              )
              (let
                (rec)
                (termbind
                  (nonstrict)
                  (vardecl
                    map
                    (all a (type) (all b (type) (fun (fun a b) (fun [List a] [List b]))))
                  )
                  (abs
                    a
                    (type)
                    (abs
                      b
                      (type)
                      (lam
                        f
                        (fun a b)
                        (lam
                          l
                          [List a]
                          [
                            [
                              [
                                { [ { Nil_match a } l ] (fun Unit [List b]) }
                                (lam thunk Unit { Nil b })
                              ]
                              (lam
                                x
                                a
                                (lam
                                  xs
                                  [List a]
                                  (lam
                                    thunk
                                    Unit
                                    [
                                      [ { Cons b } [ f x ] ]
                                      [ [ { { map a } b } f ] xs ]
                                    ]
                                  )
                                )
                              )
                            ]
                            Unit
                          ]
                        )
                      )
                    )
                  )
                )
                (let
                  (nonrec)
                  (termbind
                    (strict)
                    (vardecl tx (fun TransactionObject Transaction))
                    (lam
                      ds
                      TransactionObject
                      [
                        { [ TransactionObject_match ds ] Transaction }
                        (lam ds MultiSignature (lam ds Transaction ds))
                      ]
                    )
                  )
                  (termbind
                    (strict)
                    (vardecl
                      close
                      (fun MultisigPublicKey (fun Eta (fun Xi [Maybe Eta])))
                    )
                    (lam
                      kAgg
                      MultisigPublicKey
                      (lam
                        eta
                        Eta
                        (lam
                          xi
                          Xi
                          (let
                            (nonrec)
                            (termbind
                              (nonstrict)
                              (vardecl s (con integer))
                              [
                                { [ Xi_match xi ] (con integer) }
                                (lam
                                  u
                                  UTXO
                                  (lam
                                    s
                                    (con integer)
                                    (lam
                                      sigma
                                      MultiSignature
                                      (lam txs [List TransactionObject] s)
                                    )
                                  )
                                )
                              ]
                            )
                            (termbind
                              (nonstrict)
                              (vardecl txs [List TransactionObject])
                              [
                                { [ Xi_match xi ] [List TransactionObject] }
                                (lam
                                  u
                                  UTXO
                                  (lam
                                    s
                                    (con integer)
                                    (lam
                                      sigma
                                      MultiSignature
                                      (lam txs [List TransactionObject] txs)
                                    )
                                  )
                                )
                              ]
                            )
                            [
                              [
                                [
                                  {
                                    [
                                      Bool_match
                                      [
                                        [
                                          [
                                            {
                                              {
                                                fFoldableNil_cfoldMap
                                                [(lam a (type) a) Bool]
                                              }
                                              TransactionObject
                                            }
                                            [
                                              { fMonoidProduct Bool }
                                              fMultiplicativeMonoidBool
                                            ]
                                          ]
                                          (lam
                                            ds
                                            TransactionObject
                                            [
                                              {
                                                [ TransactionObject_match ds ]
                                                Bool
                                              }
                                              (lam
                                                ds
                                                MultiSignature
                                                (lam ds Transaction True)
                                              )
                                            ]
                                          )
                                        ]
                                        txs
                                      ]
                                    ]
                                    (fun Unit [Maybe Eta])
                                  }
                                  (lam
                                    thunk
                                    Unit
                                    (let
                                      (nonrec)
                                      (termbind
                                        (strict)
                                        (vardecl wild Bool)
                                        [ [ equalsInteger s ] (con integer 0) ]
                                      )
                                      [
                                        { Just Eta }
                                        [
                                          [
                                            [
                                              Eta
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        Bool_match
                                                        [
                                                          [ equalsInteger s ]
                                                          (con integer 0)
                                                        ]
                                                      ]
                                                      (fun Unit UTXO)
                                                    }
                                                    (lam
                                                      thunk
                                                      Unit
                                                      [
                                                        {
                                                          [ Eta_match eta ] UTXO
                                                        }
                                                        (lam
                                                          ds
                                                          UTXO
                                                          (lam
                                                            ds
                                                            (con integer)
                                                            (lam
                                                              ds
                                                              [List Transaction]
                                                              ds
                                                            )
                                                          )
                                                        )
                                                      ]
                                                    )
                                                  ]
                                                  (lam
                                                    thunk
                                                    Unit
                                                    [
                                                      { [ Xi_match xi ] UTXO }
                                                      (lam
                                                        u
                                                        UTXO
                                                        (lam
                                                          s
                                                          (con integer)
                                                          (lam
                                                            sigma
                                                            MultiSignature
                                                            (lam
                                                              txs
                                                              [List TransactionObject]
                                                              u
                                                            )
                                                          )
                                                        )
                                                      )
                                                    ]
                                                  )
                                                ]
                                                Unit
                                              ]
                                            ]
                                            s
                                          ]
                                          [
                                            [
                                              {
                                                { map TransactionObject }
                                                Transaction
                                              }
                                              tx
                                            ]
                                            txs
                                          ]
                                        ]
                                      ]
                                    )
                                  )
                                ]
                                (lam thunk Unit { Nothing Eta })
                              ]
                              Unit
                            ]
                          )
                        )
                      )
                    )
                  )
                  (termbind
                    (strict)
                    (vardecl
                      validate
                      (fun HydraState (fun HydraInput (fun ValidatorCtx Bool)))
                    )
                    (lam
                      ds
                      HydraState
                      (lam
                        input
                        HydraInput
                        (lam
                          ctx
                          ValidatorCtx
                          [
                            [
                              [
                                [
                                  [
                                    { [ HydraState_match ds ] (fun Unit Bool) }
                                    (lam thunk Unit False)
                                  ]
                                  (lam thunk Unit False)
                                ]
                                (lam thunk Unit False)
                              ]
                              (lam
                                ds
                                OpenState
                                (lam
                                  thunk
                                  Unit
                                  [
                                    { [ OpenState_match ds ] Bool }
                                    (lam
                                      ds
                                      MultisigPublicKey
                                      (lam
                                        ds
                                        Eta
                                        [
                                          [
                                            [
                                              [
                                                {
                                                  [ HydraInput_match input ]
                                                  (fun Unit Bool)
                                                }
                                                (lam
                                                  xi
                                                  Xi
                                                  (lam
                                                    thunk
                                                    Unit
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Maybe_match Eta
                                                              }
                                                              [
                                                                [
                                                                  [ close ds ]
                                                                  ds
                                                                ]
                                                                xi
                                                              ]
                                                            ]
                                                            (fun Unit Bool)
                                                          }
                                                          (lam
                                                            ds
                                                            Eta
                                                            (lam thunk Unit True
                                                            )
                                                          )
                                                        ]
                                                        (lam thunk Unit False)
                                                      ]
                                                      Unit
                                                    ]
                                                  )
                                                )
                                              ]
                                              (lam thunk Unit False)
                                            ]
                                            (lam
                                              default_arg0
                                              HeadParameters
                                              (lam thunk Unit False)
                                            )
                                          ]
                                          Unit
                                        ]
                                      )
                                    )
                                  ]
                                )
                              )
                            ]
                            Unit
                          ]
                        )
                      )
                    )
                  )
                  validate
                )
              )
            )
          )
        )
      )
    )
  )
)