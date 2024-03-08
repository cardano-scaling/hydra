import { HeadState } from "./app/model"

export const totalLovelaceValueLocked = (head: HeadState) => {
    return (head.members || []).reduce((total, member) => {
        if (member.commits && Object.keys(member.commits).length > 0) {
            return total + Object.values(member.commits).reduce((memberTotal, commit) => {
                return memberTotal + commit.value.lovelace
            }, 0)
        }
        return total
    }, 0)
}