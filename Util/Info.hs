module Util.Info where


class InfoClass i where
	name :: i -> String
	descr :: i -> String
	info :: (String,String) -> i


data TypedInfo t = TypedInfo {
	getTypedInfoName :: String,
	getTypedInfoDescr :: String
}
	deriving(Show)
instance InfoClass (TypedInfo t) where
	name = getTypedInfoName
	descr = getTypedInfoDescr
	info (name,descr) = TypedInfo name descr

-- deprecated: 
data Info = Info {
	getInfoName :: String,
	getInfoDescr :: String
}
	deriving(Show)
instance InfoClass Info where
	name = getInfoName
	descr = getInfoDescr
	info (name,descr) = Info name descr
