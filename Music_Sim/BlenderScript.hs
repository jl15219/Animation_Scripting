module BlenderScript where

transXYZ :: (Double,Double,Double) -> String
transXYZ (x,y,z) = "bpy.ops.transform.translate(value=(" ++ show x ++ "," ++ show y ++ ", " ++ show z ++ "), orient_type='GLOBAL', orient_matrix=((1, 0, 0), (0, 1, 0), (0, 0, 1)), orient_matrix_type='GLOBAL', constraint_axis=(False, False, True), mirror=True, use_proportional_edit=False, proportional_edit_falloff='SMOOTH', proportional_size=10, use_proportional_connected=False, use_proportional_projected=False, release_confirm=True)"

transZVert :: String -> Integer -> Double -> String
transZVert name vert z = "bpy.data.objects[\"" ++ name ++ "\"].data.vertices[" ++ (show vert) ++ "].co.z +=" ++ (show z)

setZVert :: String -> Integer -> Double -> String
setZVert name vert z = "bpy.data.objects[\"" ++ name ++ "\"].data.vertices[" ++ (show vert) ++ "].co.z = " ++ (show z)

transObj1D :: String -> Integer -> Integer -> String
transObj1D name dir amount = "bpy.data.objects[\"" ++ name ++ "\"].location[" ++ (show dir) ++ "] += " ++ (show amount)

pasteCopied :: String
pasteCopied = "bpy.ops.view3d.pastebuffer()"


-- Need to implement numbers
loopcuts :: Integer -> String
loopcuts x = "bpy.ops.mesh.loopcut_slide(MESH_OT_loopcut={\"number_cuts\":" ++ (show x) ++ ", \"smoothness\":0, \"falloff\":'INVERSE_SQUARE', \"object_index\":0, \"edge_index\":20, \"mesh_select_mode_init\":(True, False, False)}, TRANSFORM_OT_edge_slide={\"value\":0, \"single_side\":False, \"use_even\":False, \"flipped\":False, \"use_clamp\":True, \"mirror\":True, \"snap\":False, \"snap_target\":'CLOSEST', \"snap_point\":(0, 0, 0), \"snap_align\":False, \"snap_normal\":(0, 0, 0), \"correct_uv\":True, \"release_confirm\":True, \"use_accurate\":False})"
